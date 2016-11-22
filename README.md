dynamic-meta
============

Methods for live coding (dynamic recompilation without reloading the world) in Haskell.

## Existential.World

`ghci` + `:set -fobject-code`

Existential types (`-XExistentialQuantification`)

## Church.World

`ghci` + `:set -fobject-code`

Church encoded existential types (`-XRankNTypes` and `-XImpredicativeTypes`)

`∃ x. P(x) = ∀ r. (∀ x. P(x) → r) → r`
