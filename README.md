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
