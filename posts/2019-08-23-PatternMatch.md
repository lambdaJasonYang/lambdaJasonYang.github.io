---
title: What is Pattern matching?
tags: mathcs, functional, categorytheory, puremath
---

```hs
f x = 
case x of True  -> False
          False -> True

```

```hs
let f True  = False
    f False = True
 in f x
```