---
title: Recursion Induction Duality
tags: mathcs, prog, python
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

* In inductive proofs, $P(k-1) \overset{proves}\Rightarrow P(k)$
  * we assume `id(k-1)`{.python} is correct to design an algorithm for `id(k)`{.python}.  
* But computationally or recursively, 
`id(k-1)`{.python} $\overset{reduce}\Leftarrow$ `id(k)`{.python} 



Induction can be thought as a meta-level confirmation that computable recursion will work.

---

### Diagrams

The lifelines of the Sequence diagram(the rectangular boxes) **stacked** on each other indicates a recursive function.  
We can take advantage of this stacking behavior to implement recursion using stacks.


```plantuml
@startuml

B -> B:
activate B
B -> B:
activate B
B -> B:
activate B
B --> B:
deactivate B
B --> B:
deactivate B
B --> B:
deactivate B
@enduml
```


```plantuml
@startuml
start
if (i ) then (eq 0)
  :1;
  stop

else ( > 1) 
  :4 *;
partition subproblem {
  start

  if ( i ) then (eq 0)
  : 1;
  stop
  else (> 1)
    :4 *; 
    partition subsubproblem {
    :...;
    }

  endif
}
stop 
endif
@enduml
```