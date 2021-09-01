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
```
Proving this is simply strengthing the IH so instead of:
 $even(n) \overset{proves}\Rightarrow even(n+1)$  
we have   
 $even(n) \land odd(n) \overset{proves}\Rightarrow even(n+1) \land odd(n+1)$  

Any mutual recursive function can be inlined to the other

```python
#insert Inline odd(n) into even(n)
def Even_oddinline(n):
  if n == 0:
    return True
  else:
    if n - 1 == 0:
      return False
    else:
      return Even_oddinline((n-1)-1)
```

```python
#insert Inline even(n) into odd(n)
def Odd_eveninline(n):
  if n == 0:
    return False
  else:
    if n - 1 == 0:
      return True
    else:
      return Odd_eveninline((n-1)-1)
```