//! `$derive` blocks (v17, milestone 141): `$derive<T: I> I for T[] { … }`,
//! how an interface is met by a type it doesn't own. `$derive` lexes only in
//! bundled sources, so programs can never write one.

use super::{PResult, Parser};
use crate::ast::{DeriveDecl, DeriveTarget, Item};
use crate::lexer::TokenKind;
use crate::span::Span;

/// The word that starts a derive block.
pub(super) const DERIVE: &str = "$derive";

impl Parser<'_> {
    /// True at `$derive`.
    pub(super) fn at_derive(&self) -> bool {
        matches!(self.peek(), TokenKind::Ident(word) if word == DERIVE)
    }

    /// `$derive [<params>] Interface for target { methods }`.
    pub(super) fn derive_decl(&mut self) -> PResult<Item> {
        let start = self.bump().start;
        let type_params = self.type_params()?;
        let interface = self.ident("interface name")?;
        self.expect(&TokenKind::For)?;
        let target = match self.peek() {
            TokenKind::Struct => {
                self.bump();
                DeriveTarget::Struct
            }
            TokenKind::Enum => {
                self.bump();
                DeriveTarget::Enum
            }
            TokenKind::Ident(word) if word == "tuple" => {
                self.bump();
                DeriveTarget::Tuple
            }
            _ => DeriveTarget::Type(self.ty()?),
        };
        let open = self.expect(&TokenKind::LBrace)?;
        let methods = self.impl_methods(open)?;
        Ok(Item::Derive(DeriveDecl {
            type_params,
            interface,
            target,
            methods,
            span: Span::new(start, self.prev_end()),
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{DeriveTarget, Item, TypeKind};
    use crate::lexer::{lex, lex_bundled};
    use crate::parser::parse;

    fn items(src: &str) -> Vec<Item> {
        let (tokens, _) = lex_bundled(src).expect("lexes");
        parse(&tokens).expect("parses").0.items
    }

    #[test]
    fn a_block_for_one_type_keeps_its_parameters_and_methods() {
        let found =
            items("$derive<T: J> J for T[] { fun $a(self): i64 = 1; fun $b(x: i64): i64 = x; }");
        let [Item::Derive(d)] = found.as_slice() else {
            panic!("{found:?}")
        };
        assert_eq!(d.interface.name, "J");
        assert_eq!(d.type_params.len(), 1);
        let DeriveTarget::Type(t) = &d.target else {
            panic!("{:?}", d.target)
        };
        assert!(matches!(t.kind, TypeKind::Array(_)), "{t:?}");
        assert_eq!(d.methods.len(), 2);
        assert!(d.methods[0].receiver.is_some() && d.methods[1].receiver.is_none());
    }

    #[test]
    fn blocks_cover_every_struct_enum_or_tuple() {
        let found = items("$derive J for struct {}\n$derive J for enum {}\n$derive J for tuple {}");
        let targets: Vec<&DeriveTarget> = found
            .iter()
            .map(|i| match i {
                Item::Derive(d) => &d.target,
                other => panic!("{other:?}"),
            })
            .collect();
        assert_eq!(
            targets,
            [
                &DeriveTarget::Struct,
                &DeriveTarget::Enum,
                &DeriveTarget::Tuple
            ]
        );
    }

    #[test]
    fn programs_cant_write_one() {
        // `$` starts no identifier outside bundled sources (E0101).
        let err = lex("$derive J for struct {}").expect_err("doesn't lex");
        assert_eq!(err[0].code, "E0101");
    }
}
