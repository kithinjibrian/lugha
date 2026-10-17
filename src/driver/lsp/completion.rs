//! Completion (v16 draft decisions 28–31): what can be written at the
//! cursor, from the last check that parsed. Its offsets are trusted only
//! where its text and the current text agree (decision 29).

use std::path::Path;

use lsp_types::{
    CompletionItem, CompletionItemKind as Kind, Documentation, MarkupContent, MarkupKind, Position,
};

use super::docs;
use super::hover::{Snapshot, byte_offset, declaration};
use super::snippets;
use crate::ast::{FunDecl, Item, Program, VisibilityKind};
use crate::check::{self, Detail, Member, MemberKind, Name, Target, Type};
use crate::modules::Module;
use crate::span::Span;

/// The keywords a program may write (spec §2), less the type names, which
/// come with the types.
const KEYWORDS: [&str; 28] = [
    "fun",
    "extern",
    "struct",
    "enum",
    "impl",
    "let",
    "mut",
    "if",
    "else",
    "while",
    "for",
    "in",
    "of",
    "match",
    "import",
    "private",
    "public",
    "interface",
    "return",
    "break",
    "continue",
    "true",
    "false",
    "as",
    "self",
    "spawn",
    "select",
    "actor",
];

/// What is being typed at the cursor.
#[derive(Debug, PartialEq)]
struct Context {
    /// Where the identifier being typed starts (the cursor when there is none).
    word: usize,
    /// The `.` before it, if any.
    dot: Option<usize>,
}

/// The completions at `at` in `file`, whose current text is `text`.
pub(super) fn answer(
    snapshot: &Snapshot,
    file: &Path,
    text: &str,
    at: Position,
    snippets: bool,
) -> Vec<CompletionItem> {
    let Some(cursor) = byte_offset(text, at) else {
        return Vec::new();
    };
    let Some(context) = context(text, cursor) else {
        return Vec::new();
    };
    let Some(mut here) = Here::new(snapshot, file, text) else {
        return Vec::new();
    };
    // A `(` already there: no call snippets (milestone 78).
    here.snippets = snippets && !text[cursor..].starts_with('(');
    match context.dot {
        Some(dot) => {
            let mut items = here.members(dot);
            items.extend(snippets::postfix_match(&here, dot, at));
            items
        }
        None => here.names(context.word),
    }
}

/// The context at `cursor`, or `None` inside a comment or a string.
fn context(text: &str, cursor: usize) -> Option<Context> {
    let line_start = text[..cursor].rfind('\n').map_or(0, |i| i + 1);
    if in_comment_or_string(&text[line_start..cursor]) {
        return None;
    }
    let word = cursor
        - text[..cursor]
            .chars()
            .rev()
            .take_while(|c| is_ident(*c))
            .map(char::len_utf8)
            .sum::<usize>();
    let dot = (word > 0 && text.as_bytes()[word - 1] == b'.').then(|| word - 1);
    Some(Context { word, dot })
}

fn is_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Whether the end of `line` is inside a `//` comment or a string (strings
/// stay on one line, spec §2).
fn in_comment_or_string(line: &str) -> bool {
    let mut chars = line.chars().peekable();
    let mut string = false;
    while let Some(c) = chars.next() {
        match c {
            '\\' if string => {
                chars.next();
            }
            '"' => string = !string,
            '/' if !string && chars.peek() == Some(&'/') => return true,
            _ => {}
        }
    }
    string
}

/// How many leading bytes `a` and `b` share, at a character boundary.
fn common_prefix(a: &str, b: &str) -> usize {
    let mut n = a.bytes().zip(b.bytes()).take_while(|(x, y)| x == y).count();
    while !a.is_char_boundary(n) {
        n -= 1;
    }
    n
}

/// The cursor's file in the last good check; shared with signature help.
pub(super) struct Here<'a> {
    pub snapshot: &'a Snapshot,
    pub module: &'a Module,
    pub text: &'a str,
    /// Where the file starts in the check's offset space.
    pub base: usize,
    /// How many leading bytes the check's text and the current text share.
    pub same: usize,
    /// Completion may insert snippets (milestone 78).
    pub snippets: bool,
}

/// What a name before a `.` turned out to be.
pub(super) enum Receiver {
    /// A value's or a type's members.
    Members(Vec<Member>),
    /// The module at this path.
    Module(Vec<String>),
}

impl<'a> Here<'a> {
    /// `file`, whose current text is `text`, in `snapshot`.
    pub(super) fn new(snapshot: &'a Snapshot, file: &Path, text: &'a str) -> Option<Here<'a>> {
        let (base, old) = snapshot.base(file)?;
        let module = snapshot.modules.iter().find(|m| m.base == base)?;
        let same = common_prefix(old, text);
        Some(Here {
            snapshot,
            module,
            text,
            base,
            same,
            snippets: false,
        })
    }

    /// What can follow the `.` at `dot`.
    fn members(&self, dot: usize) -> Vec<CompletionItem> {
        match self.receiver(dot) {
            Some(Receiver::Members(members)) => self.items(&members),
            Some(Receiver::Module(path)) => self.module_items(&path),
            None => Vec::new(),
        }
    }

    /// What the name before the `.` at `dot` is: from the check's expression
    /// ending there when it is in the shared text, else by name.
    pub(super) fn receiver(&self, dot: usize) -> Option<Receiver> {
        let partial = &self.snapshot.partial;
        if self.same >= dot {
            let end = self.base + dot;
            // A module or type name written just before the dot.
            let named = partial.definitions.iter().filter(|(s, _)| s.end == end);
            if let Some(target) = smallest(named.map(|(s, t)| (*s, t))) {
                match target {
                    Target::Module(path) => return Some(Receiver::Module(path.clone())),
                    Target::Decl(decl) => {
                        if let Some(key) = self.type_at(*decl) {
                            return Some(Receiver::Members(check::type_members(partial, &key)));
                        }
                    }
                }
            }
            // An expression ending at the dot, typed by that check.
            let typed = partial.spans.iter().enumerate().filter_map(|(id, span)| {
                let span = (*span)?;
                let ty = partial.types.get(id)?.as_ref()?;
                let usable =
                    span.end == end && !matches!(ty, Type::Unit | Type::Error | Type::Never);
                usable.then_some((span, ty))
            });
            if let Some(ty) = smallest(typed) {
                return Some(Receiver::Members(check::value_members(partial, ty)));
            }
        }
        // A name just before the dot that this check may not have seen
        // there: `p.` typed on a new line.
        let name_len: usize = self.text[..dot]
            .chars()
            .rev()
            .take_while(|c| is_ident(*c))
            .map(char::len_utf8)
            .sum();
        let name_start = dot - name_len;
        let name = &self.text[name_start..dot];
        if name.is_empty() || self.text[..name_start].ends_with('.') {
            return None;
        }
        if let Some(local) = self
            .locals(name_start)
            .into_iter()
            .find(|(n, _, _)| n == name)
        {
            return Some(Receiver::Members(check::value_members(partial, &local.1)));
        }
        let program = self.program();
        if let Some(n) = program.and_then(|p| p.imports.iter().position(|i| i.bound().name == name))
        {
            let path = self.module.imports.get(n).cloned().unwrap_or_default();
            return Some(Receiver::Module(path));
        }
        let key = check::qualify(&self.module.path, name);
        if partial.members.contains_key(&key) {
            return Some(Receiver::Members(check::type_members(partial, &key)));
        }
        None
    }

    /// Every name that may start at `word`.
    fn names(&self, word: usize) -> Vec<CompletionItem> {
        let mut out: Vec<CompletionItem> = Vec::new();
        for (name, ty, mutable) in self.locals(word) {
            let mutable = if mutable { "mut " } else { "" };
            out.push(item(
                &name,
                Kind::VARIABLE,
                format!("let {mutable}{name}: {ty}"),
            ));
        }
        if let Some(program) = self.program() {
            out.extend(self.program_items(program, true));
            for import in &program.imports {
                let name = &import.bound().name;
                out.push(item(name, Kind::MODULE, format!("module {name}")));
            }
        }
        for name in check::intrinsics() {
            out.push(item(name, Kind::FUNCTION, "intrinsic".to_string()));
        }
        for name in ["Some", "None", "Ok", "Err"] {
            out.push(item(name, Kind::ENUM_MEMBER, String::new()));
        }
        for keyword in KEYWORDS {
            out.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(Kind::KEYWORD),
                ..CompletionItem::default()
            });
        }
        // An inner name hides an outer one: keep the first of each label.
        let mut seen = std::collections::HashSet::new();
        out.retain(|i| seen.insert(i.label.clone()));
        out
    }

    /// The locals and parameters declared before `limit` (a current-text
    /// offset) in the function around it, newest first: only those inside
    /// the text the check shares with the current one.
    pub(super) fn locals(&self, limit: usize) -> Vec<(String, Type, bool)> {
        let shared = self.base + self.same.min(limit);
        let Some(function) = self.program().and_then(|p| enclosing(p, shared)) else {
            return Vec::new();
        };
        let mut found: Vec<(Span, String, Type, bool)> = self
            .snapshot
            .partial
            .names
            .iter()
            .filter_map(|(span, name)| match name {
                Name::Local { name, ty, mutable }
                    if span.start >= function.start && span.end <= shared =>
                {
                    Some((*span, name.clone(), ty.clone(), *mutable))
                }
                _ => None,
            })
            .collect();
        found.sort_by_key(|(span, ..)| std::cmp::Reverse(span.start));
        found.into_iter().map(|(_, n, t, m)| (n, t, m)).collect()
    }

    pub(super) fn program(&self) -> Option<&Program> {
        self.module.program.as_ref()
    }

    /// The struct, enum or interface whose name is declared at `decl`.
    fn type_at(&self, decl: Span) -> Option<String> {
        let members = &self.snapshot.partial.members;
        members
            .iter()
            .find(|(_, t)| t.decl == decl)
            .map(|(k, _)| k.clone())
    }

    /// The items of the module at `path` that this module may use.
    fn module_items(&self, path: &[String]) -> Vec<CompletionItem> {
        let Some(module) = self.snapshot.modules.iter().find(|m| m.path == path) else {
            return Vec::new();
        };
        let mut out = module
            .program
            .as_ref()
            .map_or_else(Vec::new, |p| self.program_items(p, false));
        out.extend(self.reexported_items(module, &mut Vec::new()));
        out
    }

    /// A program's functions and types; `private` ones only for its own module.
    pub(super) fn program_items(&self, program: &Program, own: bool) -> Vec<CompletionItem> {
        let shown = |v: &Option<crate::ast::Visibility>| {
            own || v.as_ref().is_none_or(|v| v.kind != VisibilityKind::Private)
        };
        let mut out = Vec::new();
        for it in &program.items {
            let (name, kind, detail) = match it {
                Item::Fun(f) if shown(&f.visibility) && f.name.name != "main" => (
                    &f.name,
                    Kind::FUNCTION,
                    self.header(f.name.span, &f.name.name),
                ),
                Item::Extern(e) if shown(&e.visibility) => (
                    &e.name,
                    Kind::FUNCTION,
                    format!("extern fun {}", e.name.name),
                ),
                Item::Struct(s) if shown(&s.visibility) => {
                    (&s.name, Kind::STRUCT, format!("struct {}", s.name.name))
                }
                Item::Enum(e) if shown(&e.visibility) => {
                    (&e.name, Kind::ENUM, format!("enum {}", e.name.name))
                }
                Item::Interface(i) if shown(&i.visibility) => (
                    &i.name,
                    Kind::INTERFACE,
                    format!("interface {}", i.name.name),
                ),
                Item::ExternType(t) if shown(&t.visibility) => (
                    &t.name,
                    Kind::STRUCT,
                    format!("extern type {}", t.name.name),
                ),
                _ => continue,
            };
            out.push(self.documented(item(&name.name, kind, detail), Some(name.span)));
        }
        out
    }

    /// `item` with the doc comment of the declaration named at `decl`
    /// (milestone 77), if it has one.
    fn documented(&self, mut item: CompletionItem, decl: Option<Span>) -> CompletionItem {
        let doc = decl
            .and_then(|d| docs::doc_for(self.snapshot, d))
            .map(|d| d.markdown());
        item.documentation = doc.filter(|d| !d.is_empty()).map(|value| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value,
            })
        });
        snippets::call(self, &mut item, decl);
        item
    }

    fn items(&self, members: &[Member]) -> Vec<CompletionItem> {
        members
            .iter()
            .map(|m| {
                let kind = match m.kind {
                    MemberKind::Field => Kind::FIELD,
                    MemberKind::Method => Kind::METHOD,
                    MemberKind::Associated => Kind::FUNCTION,
                    MemberKind::Variant => Kind::ENUM_MEMBER,
                };
                let detail = match &m.detail {
                    Detail::Type(ty) => format!("{}: {ty}", m.name),
                    Detail::Decl(decl) => self.header(*decl, &m.name),
                    Detail::Text(text) => text.clone(),
                };
                self.documented(item(&m.name, kind, detail), m.decl)
            })
            .collect()
    }

    /// A function's header as written, or its name when it has none.
    pub(super) fn header(&self, decl: Span, name: &str) -> String {
        declaration(self.snapshot, decl).map_or_else(|| name.to_string(), |(header, _)| header)
    }
}

/// The function or method whose body holds `offset`.
fn enclosing(program: &Program, offset: usize) -> Option<Span> {
    let holds = |f: &FunDecl| f.span.start <= offset && offset <= f.span.end;
    program.items.iter().find_map(|it| match it {
        Item::Fun(f) if holds(f) => Some(f.span),
        Item::Impl(i) => i.methods.iter().find(|m| holds(m)).map(|m| m.span),
        _ => None,
    })
}

/// The value of the smallest span among `found`.
fn smallest<T>(found: impl Iterator<Item = (Span, T)>) -> Option<T> {
    found
        .min_by_key(|(s, _)| (s.end - s.start, s.start))
        .map(|(_, t)| t)
}

fn item(label: &str, kind: Kind, detail: String) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(kind),
        detail: (!detail.is_empty()).then(|| crate::check::shown(&detail)),
        ..CompletionItem::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_context_is_the_word_and_the_dot_before_it() {
        assert_eq!(
            context("p.", 2),
            Some(Context {
                word: 2,
                dot: Some(1)
            })
        );
        assert_eq!(
            context("  p.le", 6),
            Some(Context {
                word: 4,
                dot: Some(3)
            })
        );
        assert_eq!(
            context("let x = tot", 11),
            Some(Context { word: 8, dot: None })
        );
        assert_eq!(context("x\n", 2), Some(Context { word: 2, dot: None }));
    }

    #[test]
    fn nothing_completes_in_comments_and_strings() {
        assert_eq!(context("// p.", 5), None);
        assert_eq!(context("let s = \"p.", 11), None);
        assert_eq!(context("let s = \"a\\\" p.", 15), None);
        assert!(context("let s = \"a\"; p.", 15).is_some());
        assert!(context("let s = \"//\"; p.", 16).is_some());
    }

    #[test]
    fn the_shared_prefix_stops_at_the_first_difference() {
        assert_eq!(common_prefix("abc", "abd"), 2);
        assert_eq!(common_prefix("abc", "abc"), 3);
        assert_eq!(common_prefix("é", "è"), 0);
    }

    #[test]
    fn keywords_are_the_lexers() {
        for k in KEYWORDS {
            assert!(
                crate::lexer::token::keyword(k).is_some(),
                "{k} isn't a keyword"
            );
        }
    }
}
