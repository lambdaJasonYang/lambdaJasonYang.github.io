---
title: Recursion Induction Duality
tags: tech
---

> Recursion is informally the Computational Dual of Induction.  
> Induction tells us some chain of increasing P(n) is true.    
> Recursion uses that knowledge of induction to safely climb down that chain of truths to the base case.  
> -me  

Induction comes from your thoughts and algorithm design.  
Recursion comes from the code being executed.  

``` python
def identity(k):
    if k == 0:
        return 0
    if k == 1:
        return 1
    else:
        return 1+identity(k-1)

```

* In inductive proofs, $P(k-1) \overset{proves}\rightarrow P(k)$
  * we assume `id(k-1)`{.python} is correct to design an algorithm for `id(k)`{.python}.  
* But computationally or recursively, 
`id(k-1)`{.python} $\overset{reduce}\leftarrow$ `id(k)`{.python} 



### Analogy

Lets pretend we have Wizard, Worker and stairs.   
Whatever Wizard says must be true in this reality, nothing more, nothing less. (Don't assume that a ground exists or that we can freely go up or down the stairs)

Wizard is on the ground at the bottom of the stairs.  
Worker is at some position on the stairs but needs to get his tools on the ground.  

* Induction - Wizard at the bottom of a stairs saying: 
  * "I know that the ground exists and we can take a step upwards from the ground to the stairs or from the current step to the next higher step"
  * "Therefore I am certain there is a chain of steps to your(Worker) position"
* Recursion - Worker at some position on the stairs who gets the memo and thinks: 
  * "This means the wizard is certain there is a path from the ground to my position; Then I must be able to step down to the ground, get my tools, then come back up here"

When induction is done incorrectly meaning the Wizard misspoke and conjured up a different reality by accident.  

* The Wizard is floating and tells us the ground doesn't exist.
    * Worker will climb down infinitely
* The Wizard tells us we can only take the current step to a step downwards.
    * Worker looks down and sees NOTHING, BUT he looks upwards and sees steps upwards.
      * Reasoning: The only way to get to Worker's position is by stepping downwards from a higher position. This means the only way for the Worker to move is upwards.
      * Worker will climb up infinitely



