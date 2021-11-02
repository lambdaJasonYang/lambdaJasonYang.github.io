---
title: Dependencies, Arrows and Confusion
tags: mathcs, puremath, recursion
---

### Pure Functions

AST is used for functional programming

```python
def plus(x,y):
    return x + y
```
 * plus is pure
 * plus is pass-by-value 
   * plus does not modify args, x y

```plantuml
digraph G{

    plus
    plus -> x [color = red]
    plus -> y [color = red]
}
```
**The arrows in red DO NOT REPRESENT dependency like in OO or UML diagram**

In OO, the same diagram does not exist because the arrows now represent object dependency.



Therefore plus does not have an object dependency diagram.

---

### OO

#### pointers

```C
int *a = &b;
```

```C
void plus(&x,&y,&z):
    z = x + y
```