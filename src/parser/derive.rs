//! Derive blocks (v17, milestones 141–142): `derive<T: I> I for T[] { … }`,
//! how an interface is met by types its module doesn't declare. `derive` is
//! a contextual word: it starts a block only at the start of an item,
//! followed by `<` or a name; anywhere else it is an ordinary name.

use super::{PResult, Parser};
use crate::ast::{DeriveDecl, DeriveTarget, Item};
use crate::lexer::TokenKind;
use crate::span::Span;

/// The word that starts a derive block.
pub(super) const DERIVE: &str = "derive";

impl Parser<'_> {
    /// True at `derive` followed by `<` or a name: a block, not a name.
    pub(super) fn at_derive(&self) -> bool {
        matches!(self.peek(), TokenKind::Ident(word) if word == DERIVE)
            && matches!(self.peek_at(1), TokenKind::Lt | TokenKind::Ident(_))
    }

    /// `derive [<params>] Interface for target { methods }`.
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
    use crate::parser::test_util::parse_src;

    fn items(src: &str) -> Vec<Item> {
        parse_src(src).unwrap_or_else(|d| panic!("{d:?}")).items
    }

    #[test]
    fn a_block_for_one_type_keeps_its_parameters_and_methods() {
        let found =
            items("derive<T: J> J for T[] { fun a(self): i64 = 1; fun b(x: i64): i64 = x; }");
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
        let found = items("derive J for struct {}\nderive J for enum {}\nderive J for tuple {}");
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
    fn derive_is_a_name_everywhere_else() {
        let found = items(
            "struct S { derive: i64 }\n\
             fun derive(derive: i64): i64 { let derive = derive + 1; derive }\n\
             fun f(s: S): i64 = derive(s.derive);",
        );
        assert_eq!(found.len(), 3);
        assert!(found.iter().all(|i| !matches!(i, Item::Derive(_))));
    }
}
