---
title: For Loops, Tail recursion, and Left fold
tags: tech
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

It will ask you to login with browser or use a personal access token.
Personal access token can be created at
Settings >> Developer Settings >> Personal Access tokens

