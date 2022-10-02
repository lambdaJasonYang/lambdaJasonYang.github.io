---
title: Applicative
tags: mathcs, functional
---

```hs
class (Functor fun) => Applicative fun where
pure :: a -> fun a
(<*>) :: fun (a -> b) -> fun a -> fun b
```


# Map vs Applicative

* Notice Applicative is like fmap but gives higher precision than fmap.

```hs
fmap (*2) [1,2,3,4]
<*> [(+3) (*4) (*3) (+5)] [1,2,3,4]
```



