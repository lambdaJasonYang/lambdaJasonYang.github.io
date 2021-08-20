---
title: Mutual recursion
tags: tech,mathcs,AI,musings
---

``` python
def even(n):
  if n == 0:
    return True
  else:
    return odd(n-1)

def odd(n):
  if n == 0:
    return False
  else:
    return even(n-1)

#any mutual recursive function can be inlined to the other
def Even_oddinline(n):
  if n == 0:
    return True
  else:
    if n - 1 == 0:
      return False
    else:
      return Even_oddinline((n-1)-1)

def Odd_eveninline(n):
  if n == 0:
    return False
  else:
    if n - 1 == 0:
      return True
    else:
      return Odd_eveninline((n-1)-1)
```