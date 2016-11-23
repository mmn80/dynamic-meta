dynamic-meta
============

Methods for live coding (dynamic recompilation without rebooting the world) in Haskell.

`ghci` + `:set -fobject-code`

## Existential.World

Existential types

Internal type `SomeEntity` declared with the help of
`-XExistentialQuantification` and manipulated generically with
`-XRankNTypes` functions

```Haskell
data SomeEntity = forall e. Entity e => SomeEntity e
```

## GADT.World

GADT encoded existential types

Identical with previous except we use `-XGADTs` instead of
`-XExistentialQuantification` for the definition of `SomeEntity`

```Haskell
data SomeEntity where
  SomeEntity :: forall e. Entity e => e -> SomeEntity
```

## Church.World

Church encoded existential types (`-XRankNTypes` and `-XImpredicativeTypes`)

`∃ x. P(x) = ∀ r. (∀ x. P(x) → r) → r`

```Haskell
data Entity e = Entity e (e -> Int) (Int -> e -> e)

type SomeEntity = forall x. (forall e. Entity e -> x) -> x
```

## Dynamic.World

`Data.Dynamic` encoding (`Typeable` + `Any` + `unsafeCoerce`)

We need to keep the object _and_ every method separately wrapped in `Dynamic`,
and then use `dynApp` to write abstract functions.
`dynApp` does dynamic type checking under the hood.

```Haskell
{-# LANGUAGE ScopedTypeVariables #-} 

data SomeEntity = SomeEntity Dynamic Dynamic Dynamic

mkSomeEntity :: forall e. Entity e => e -> SomeEntity
mkSomeEntity e = SomeEntity (toDyn e) (toDyn (_health :: e -> Int))
                                      (toDyn (_attack :: Int -> e -> e))
```

`Dynamic` works like a built in dependent pair indexed by `Typeable`.

## Idris

In the `srcidr` folder there is an **Idris** implementation with dependent types.

We use dependent pairs (the analog for existential quantification), and the
generic functions just take an explicit extra argument `(e : Type)` in order
to force them to be polymorphic.

```Idris
HList : Type
HList = List (e : Type ** Entity e)

foldWorld : Monoid m => HList -> ((e : Type) -> Entity e -> m) -> m
foldWorld w f = foldl f' neutral w
  where f' r (a ** ent) = r <+> f a ent
```
