Created at: 2024-11-01 14:01

# github hm

## Core Concepts

1. Constraint collection: The implementation uses a bottom-up approach for gathering constraints and assumptions, with only the monomorphic set M creation being top-down. The main function `collect` traverses the AST and returns:
	- A type
	- A list of assumptions
	- A list of constraints
By top down it means a monomorphic set is set before visiting the node while bottom up is after visiting the node

2. Data structures
	### Types
```ocaml
type ty = 
| TVar of var             (* Type variable *)
| TFun of ty * ty         (* Function type *)
| Tcon of string *ty lits (* Type constructor *)
```

### Constraints
Three main types of constraints
- Equality
- Implicit instance: type instance is generalized with a set of monomorphic variables.
- Explicit instance: type must match a polymorphic type scheme.

```ocaml
type constr =
  | Equality of ty * ty
  | ImplInstance of ty * ty * var list
  | ExplInstance of ty * scheme
```
	
Assumptions
Tracks variable-type associations during inference

3. Expression Handling

Variables
- Generate fresh type variable B
- Creates assumptions linking variable name to B
- Return empty constraint list

Function application

```ocaml
| App (e1, e2) -> 
	let beta = var_fresh () 
	and t1, a1, c1 = collect m e1 
	and t2, a2, c2 = collect m e2 in 
	(TVar beta, a1 @ a2, Equality (t1, TFun (t2, TVar beta)) :: (c1 @ c2))
```

- Creates a fresh type variable for result
- Collects constraints from both expressions
- Adds constraint ensuring e1's type is function from e2's type to result

Lambda abstractions
```ocaml
| Abs (x, e) -> 
	let beta = var_fresh () in 
	let t, a, c = collect (Set.add beta m) e in 
	let a', c' = 
		List.fold_left 
		(fun (acc, acc') a' -> 
		let (Assumption (x', t')) = a' in 
		if x' = x then (acc, Equality (t', TVar beta) :: acc') 
		else (Assumption (x', t') :: acc, acc')) 
		([], c) a 
	in
	(TFun (TVar beta, t), a', c')
```

- Creates fresh type variable for bound variable
- Adds to monomorphic set M
- Generates equality constraints for bound variable
- Removes assumptions about bound variable

Let expressions
```ocaml
| Let (x, e1, e2) ->
    let t1, a1, c1 = collect m e1 and t2, a2, c2 = collect m e2 in
    let m' = Set.to_seq m |> List.of_seq in
    let a', c' =
      List.fold_left
        (fun (acc, acc') a' ->
          let (Assumption (x', t')) = a' in
          if x' = x then (acc, ImplInstance (t', t1, m') :: acc')
          else (Assumption (x', t') :: acc, acc'))
        ([], []) a2
    in
    (t2, a1 @ a', c1 @ c2 @ c')
```

- Introduces polymorphism via implicit instance constraints
- Uses monomorphic set M to track non-generalizable variables
- Removes assumptions about bound variable

# Authors
Kithinji Brian

# References
[Github](https://github.com/bynect/constraint-inference/blob/main/infer.ml)