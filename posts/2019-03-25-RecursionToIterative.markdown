---
title: Convert recursion to Iteration
tags: tech,mathcs,AI,musings
---

``` python
def recfac(n):
  if n == 0:
    return 1
  else:
    return n * recfac(n-1)

#assume r is fac(n-1)
#design facInc to equal fac(n)
#facInc = n * fac(n-1)
def facInc(n,r):
  return n*r

#basecase is i == 0 then return r = 1
#facInc(i,r) is i * fac(i-1)
#when i == n, it becomes fac(n,r) = n * fac(n-1)
def fac3(n):
  i = 0
  r = 1
  while(i != n):
    i = i + 1
    r = facInc(i,r)
  return r

#due to associativity
def fac4(n):
  i = 0
  r = 1
  x = n
  while(i != x):
    r = facInc(r,x)
    x = x-1
  return r

####

def foo(x):
  if x > 1:
    if x <= 50:
      return 4
    else: 
      return x * x + foo(x-7)
  else: 
    return 20

def fooinc(i,r):
  return i*i+r

def foo1(x):
  xa = x
  r = None
  #first while finds the correct base case
  while True:
    if xa > 1:
      if xa <= 50:
        r = 4
        break
      else:
        xa = xa - 7
    else:
      r = 20
      break
  #second while builds from the base to answer
  while (xa != x):
    xa = xa + 7
    r = fooinc(x,r)
  return r
#####################

def sqrlist(x):
  if x == []:
    return []
  else:
    return [(x[0]*x[0])] + sqrlist(x[1:]) 
#decrement is cdr or x[1:]
#x_o = x$y = cons(y,x) 

def sqrlist0(x):
  xa = x
  stack = []
  r = None
  while True:
    if xa == []:
      r = []
      break
    else:
      stack = [xa[0]] + stack
      xa = xa[1:]

  while xa != x:
    xa = [stack[0]] + xa 
    stack = stack[1:]
    r = [xa[0]*xa[0]]+r
  return r
```