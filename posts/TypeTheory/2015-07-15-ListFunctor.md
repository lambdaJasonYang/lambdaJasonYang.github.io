---
title: List Functors Lambda Laziness
tags: mathcs
---

# List Projection is same as Lazy Lambda

```.py
x = [5]
x[0] #list projection

x = lambda (): 5
x() #lazy lambda
```


```.hs

select0 :: list int -> int
select0 x = x[0] 
#select0 x will output x[0]

lazylambda :: (() -> int) -> int
lazylambda x = x()
#lazylambda x will output x()

#GENERALIZATION
selectF :: F int -> int

F X = list X 
F X = () -> X
```

**We can treat a list projection in the same way as a lazy lambda**


