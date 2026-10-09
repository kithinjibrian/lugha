//! The spec §10 and §11 programs, extracted from the spec `lughac spec`
//! prints (`lugha::SPEC`), with the results their sentences state (PRP-016).
//! Editing the spec changes what the tests run.

// Shared by several test crates; each uses a different subset.
#![allow(dead_code)]

/// A §10 program and what the spec says it does.
pub struct SpecProgram {
    pub title: String,
    pub source: String,
    /// Every expected output ends with a newline (spec §10); empty if none.
    pub stdout: String,
    pub exit: i32,
    /// The expected diagnostics of a rejected program.
    pub stderr: Option<String>,
}

/// The text between the heading starting `start` and the next `## ` heading.
fn section(start: &str) -> &'static str {
    let from = lugha::SPEC
        .find(start)
        .unwrap_or_else(|| panic!("the spec has a `{start}` section"));
    let text = &lugha::SPEC[from..];
    let end = text[3..].find("\n## ").map_or(text.len(), |i| i + 3);
    &text[..end]
}

/// The contents of every fenced block in `text`, in order.
fn fenced_blocks(text: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let mut current: Option<String> = None;
    for line in text.lines() {
        if line.starts_with("```") {
            match current.take() {
                Some(block) => blocks.push(block),
                None => current = Some(String::new()),
            }
        } else if let Some(block) = current.as_mut() {
            block.push_str(line);
            block.push('\n');
        }
    }
    blocks
}

/// The number after "exits with " in `sentence`.
fn exit_code(sentence: &str) -> i32 {
    let rest = &sentence[sentence.find("exits with ").expect("states an exit code") + 11..];
    let digits: String = rest.chars().take_while(char::is_ascii_digit).collect();
    digits.parse().expect("the exit code is a number")
}

/// "Prints `a` then `b` on two lines, exits with 0." → "a\nb\n".
fn printed(sentence: &str) -> String {
    let Some(start) = sentence.find("Prints ") else {
        return String::new();
    };
    let end = sentence
        .find(", exits with")
        .expect("a Prints sentence states an exit code");
    let spans: Vec<&str> = sentence[start..end].split('`').skip(1).step_by(2).collect();
    spans.iter().map(|s| format!("{s}\n")).collect()
}

/// Every §10 program: a `**Title** (milestone N).` paragraph, its source
/// block, and for a rejected program a second block with its diagnostics.
pub fn section_10() -> Vec<SpecProgram> {
    let text = section("## 10.");
    let mut programs = Vec::new();
    let paragraphs: Vec<&str> = text.split("\n**").skip(1).collect();
    for paragraph in paragraphs {
        let Some((title, rest)) = paragraph.split_once("** (milestone") else {
            continue;
        };
        let sentence = rest.lines().next().expect("the paragraph has a sentence");
        let blocks = fenced_blocks(rest);
        let source = blocks
            .first()
            .unwrap_or_else(|| panic!("`{title}` has a program"))
            .clone();
        let stdout = printed(sentence);
        let stderr = if stdout.is_empty() {
            blocks.get(1).cloned()
        } else {
            None
        };
        programs.push(SpecProgram {
            title: title.to_string(),
            source,
            stdout,
            exit: exit_code(sentence),
            stderr,
        });
    }
    programs
}

/// The §11 milestone 2 program and the exit code the milestone table gives it.
pub fn milestone_2() -> SpecProgram {
    let text = section("## 11.");
    let after = &text[text
        .find("The milestone 2 test program")
        .expect("§11 has the milestone 2 program")..];
    let source = fenced_blocks(after)
        .into_iter()
        .next()
        .expect("followed by its source");
    let row = &text[text
        .find("The program below exits with")
        .expect("the table states its exit code")..];
    SpecProgram {
        title: "milestone 2".into(),
        source,
        stdout: String::new(),
        exit: exit_code(row),
        stderr: None,
    }
}

/// The JSON diagnostic line shown in §9 for the rejected program.
pub fn json_diagnostic() -> String {
    let text = section("## 9.");
    let after = &text[text
        .find("**JSON diagnostics.**")
        .expect("§9 shows a JSON line")..];
    fenced_blocks(after)
        .into_iter()
        .next()
        .expect("followed by the line")
}

/// `(title, source)` of every §10 and §11 program.
pub fn all() -> Vec<(String, String)> {
    section_10()
        .into_iter()
        .chain([milestone_2()])
        .map(|p| (p.title, p.source))
        .collect()
}

/// The source of the §10 program titled `title`.
pub fn source(title: &str) -> String {
    section_10()
        .into_iter()
        .find(|p| p.title == title)
        .unwrap_or_else(|| panic!("§10 has `{title}`"))
        .source
}
