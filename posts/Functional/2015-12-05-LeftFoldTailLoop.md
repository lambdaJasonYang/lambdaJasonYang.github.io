---
title: For Loops, Tail recursion, and Left fold
tags: mathcs, prog, functional
---


``` python
#for loop is simply a tailrecursive function
def forF(n,i,f,state):
    if i == n:
        return state
    else:
        nextStep = i+1
        return forF(n,nextStep,f,f(i,state))

xstate = {'total':0}
xstateTrans = lambda step,state: {'total': state['total'] + 1}
finalState = forF(5,0,xstateTrans,xstate)
print(finalState)

forL = lambda *x : x
```
Iterative functions are best captured by tail-recursive functions since tail-recursive functions pass their state using arguments.

Left fold can also capture this iterative process, with the base case acting as the initial state.