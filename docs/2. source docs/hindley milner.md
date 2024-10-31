Created at: 2024-10-30 15:42

# hindley milner

## Types

```haskell
newtype TVar = TV String
	deriving (Show, Eq, Ord)

data Type
	= TVar TVar
	| TCon String [Type]
```

## Type schemes
Type schemes model polymorphic types, they indicate that the type variables bound in quantifier are polymorphic across the enclosed type and can be instantiated with any type consistent with the signature. Intuitively the indicate that the implementation of the function.

```haskell
data Scheme = Forall [TVar] Type

let id = (x. x) in ...

Forall ([a], a -> a)
```


## Context
The typing context or environment is the central container around which all information during the inference process is stored and queried. In Haskell our implementation will simple be a newtype wrapper around a Map of `Var` to `Scheme` types.

```haskell
newtype TypeEnv = TypeEnv (Map.map Var Scheme)
```

The two primary operations are extension and restriction which introduce or remove named quantities from the context.

## Inference

## Substitution

```haskell
type Subst = Map.map TVar Type
```

## Constraint generation


## Unification

## Generalization
## Let polymorphism


# Authors
Kithinji Brian

# References
[smunix](https://smunix.github.io/dev.stephendiehl.com/fun/006_hindley_milner.html)
[papl](https://papl.cs.brown.edu/2014/Type_Inference.html)
[web archive](https://web.archive.org/web/20180507052411/http://www.calebh.io/Type-Inference-by-Solving-Constraints/)