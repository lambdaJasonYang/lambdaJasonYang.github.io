---
title: Recursion - Slicing the tail of list is impure
tags: mathcs, prog, functional
---

One of the most common recursive methods is to slice the list tail
```python
def fun(arr,x):
    head = arr[0]
    tail = arr[1:]
    if Prop(head,x):
        return DoSomething(head)
    else:
        IH = fun(tail,x)
        return IH
```

It works most of the time...until it doesn't.

Problem Statement: Find the index of element "x" in a list "arr" (using recursion)

The naive solution is :  
 
*  IH: Finds the element x in the tail arr and return it's index  
*  Solve: Check if x in head arr return index 0, else IH.  

Why doesn't this work?

```python
def func(arr,x):
    HeadValue = arr[0]
    tail = arr[1:]
    HeadIndex = 0
   
    if HeadValue == x:
        return HeadIndex
    else:
        #this will always return 0
        IH = func(tail,x) 
        return IH
```


`tail = arr[1:]`{.python} is impure.  
It shifts the index of `arr[1:]`{.python} by `-1`{.python}.  
**Remediate the Side-Effect**: `IH = 1 + func(tail,x)`  



Error is due to assumption that lists are pure data structs and the INDEX-VALUE relation is immutable.
What we really wanted was a dictionary.  
The below code does work when we convert list to dict.   
The partition between head and tail of the dict maintains the INDEX-VALUE pairing.


```python
arr = [5,8,2,5,24]
ConvertListToDict = lambda xs: dict(enumerate(xs))
#ConvertListToDict([5,8,3]) will return {0:5, 1:8, 2:3}

def findElemByIndex(arrX,x):
    arr = ConvertListToDict(arrX.copy())
    Indices = list(arr.keys())
    HeadIndex = Indices.pop()
    HeadValue = arr.pop(HeadIndex)
    tail = arr
    ###
    if HeadValue==x:
        return HeadIndex
    else:
        IH = findElemByIndex(tail,x)
        return IH
```