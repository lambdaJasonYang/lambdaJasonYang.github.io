---
title: Fold and Induction
tags: mathcs, recursion, prog, functional
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
recurse is a left semigroup action which is easily inferred by it's type.

$$foldr\ recurse\ base\ listarg = \begin{cases} base& \text{if listarg match empty list} \\ recurse\ listarg[0]\ (foldr\ recurse\ base\ listarg[1:]) & else \end{cases}$$



```python

def sum1(a,b):
    return a + b
print("fold sum monoid act: \t\t",fold(sum1,0,tlist))
def product1(a,b):
    return a * b
print("fold product monoid act: \t",fold(product1,1,tlist))
def and1(a,b):
    return a and b
print("fold AND monoid act: \t\t", fold(and1,True,tlist))
def or1(a,b):
    return a or b
print("fold OR monoid act: \t\t",fold(or1,False,tlist))
```
```code
fold sum monoid act: 		 15
fold product monoid act: 	 120
fold AND monoid act: 		 True
fold OR monoid act: 		 1

```


```python

appendlambda = lambda x,n: [x] + n
print("fold append monoid act: \t", fold(appendlambda,[],tlist))
#fold(lambda x. lambda y. [x] ++ y,[],
lenlambda = lambda x,n: 1 + n
print("fold length monoid act: \t",fold(lenlambda, 0, tlist))
revlambda = lambda x,xs: xs + [x]
print("fold reverse monoid act: \t",fold(revlambda,[],tlist))
maplambda = lambda f:(lambda x,xs: [f(x)] + xs )
times2 = lambda x: x * 2
print("fold map monoid act: \t\t",fold(maplambda(times2),[],tlist))

filterlambda = lambda p: (lambda x,xs: [x] + xs if p(x) else xs)
evenQ = lambda x: x%2 == 0
print("fold even monoid act: \t\t",fold(filterlambda(evenQ),[],tlist))
```

```code
fold append monoid act: 	 [1, 2, 3, 4, 5]
fold length monoid act: 	 5
fold reverse monoid act: 	 [5, 4, 3, 2, 1]
fold map monoid act: 		 [2, 4, 6, 8, 10]
fold even monoid act: 		 [2, 4]
```

### Universal property

In Hutton's paper he tells us when fold can be used. Essentially it's just an inductive proof of correctness packaged into the function g.

$$   \begin{aligned}
    g  \ []  &=  v\\
    g \ (x:xs) &=  f\ x\ (g\ xs) 
  \end{aligned}
  \quad\Longleftrightarrow\quad
  \begin{aligned}
    g &= fold\ f\ v 
  \end{aligned} $$

We can design a fold iff we can design a function g.  
The left hand side of the iff is just an inductive proof over length of list.

* g [] = v
  * show g applied to base case returns expected base case v
* (g xs) is our IH  
  * f x (g xs) = f x IH, meaning when we apply our function f to our inductive hypothesis IH and the dangling element x, we will return our expected results.

### Fusion
When can we some arbitrary function h with a fold to return another fold?
$$ h \circ fold\ a\ b \overset{?}{=} fold\ c\ d$$

We denote the 2 folds in the eqn above:  

* LHS-fold
  * basecase is b
  * recursive operation is a
* RHS-fold
  * basecase is d
  * recursive operation is c

* h b = d
* h (c x LHS-IH) = c x (a LHS-IH) 
example  

$$ (+1) \circ sumfold = fold\ (+)\ 1 $$
  
h = (+1)  
a = (+)  
b = 0  
c = (+)  
d = 1  