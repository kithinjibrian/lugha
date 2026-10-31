//! `json.Json` (v17, milestones 139–141): the interface `json.encode` and
//! `json.decode` take, met through `std.$json_value`'s derive blocks
//! (`derive.rs`, PRP-172). What's left here is JSON's own wording: the
//! bound's key and what E0450 calls a part that isn't data.

use super::Type;
use super::derive_meets::Unmet;

/// `std.json`'s `Json`, declared in `std.$json_value`.
pub(super) const BOUND: &str = "std.$json_value.Json";

/// What E0450 says a part is when it isn't JSON-able (milestone 139's wording).
pub(super) fn describe(why: &Unmet) -> String {
    match why {
        Unmet::Unbounded(name) => {
            format!("a type parameter without `json.Json` (write `{name}: json.Json`)")
        }
        Unmet::Bound(_, key) => format!("a map with `{key}` keys"),
        Unmet::Union => "a struct holding a union".to_string(),
        Unmet::Mismatch(problem) => problem.clone(),
        Unmet::Missing(ty) => match ty {
            Type::Fun(..) => "a function".to_string(),
            Type::Interface(..) => "an interface value".to_string(),
            Type::Ref(_) => "a `Ref`".to_string(),
            Type::Task(_) => "a task".to_string(),
            Type::Chan(_) => "a channel".to_string(),
            Type::Actor(_) => "an actor's handle".to_string(),
            Type::Handle(_) => "a C handle".to_string(),
            other => format!("`{other}`"),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::describe;
    use crate::check::Type;
    use crate::check::derive_meets::Unmet;

    #[test]
    fn each_miss_reads_as_milestone_139_wrote_it() {
        let f = Type::Fun(vec![Type::I64], Box::new(Type::I64));
        assert_eq!(describe(&Unmet::Missing(f)), "a function");
        assert_eq!(
            describe(&Unmet::Bound("k".into(), Type::Bool)),
            "a map with `bool` keys"
        );
        assert_eq!(
            describe(&Unmet::Unbounded("T".into())),
            "a type parameter without `json.Json` (write `T: json.Json`)"
        );
        assert_eq!(describe(&Unmet::Union), "a struct holding a union");
    }
}
