---
title: Fold and Induction
tags: mathcs
---

A literature review of Folds with Hutton et al.

### Foldr

```haskell
foldr :: (a -> b -> b) -> b -> ([a] -> b)
type recurse = (a -> b -> b)
type base = b
type listarg = [a]
type output = b
foldr :: recurse -> base -> listarg -> output
```
recurse is a left semigroup action.
