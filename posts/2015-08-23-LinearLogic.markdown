---
title: Linear Logic
tags: mathcs, logic
---

#### Prelude

$$\Delta \vdash A$$
$\Delta$ is a resource that can be consumed to produce goal A. 

Coin Exchange example:

* d,d,n (dime,dime,nickel) is our resources $\Delta$
* q (quarter) is what is produced by using up those resources  

Sequent form
$$ d,d,n \vdash q $$

Tree form
$$ \cfrac{(d\qquad d\qquad n)^{\Delta}}{q} $$ 


   
Invalid example:  
 
$$ \xcancel{\cfrac{\cfrac{(d\qquad d\qquad n)^{\Delta}}{q} \qquad \cfrac{(d\qquad d\qquad n)^{\Delta}}{q} }{h}} $$
Above would be perfectly valid in typical ND but not in linear logic. We used resource $\Delta$ twice which is not allowed.

Graph Walk Example:

If we are given Resource Node(A) and all edges of the graph Edge(A,B), Edge(B,A), Edge(A,C) ... we can walk though the entire graph using up alledges.   
${\color{red}\Delta = Node(A),Edge(A,B),Edge(B,A),Edge(A,C)}$

with a linear rule 

$$ \cfrac{Node(x) \qquad Edge(x,y)}{Node(y)}$$ 

Walk the entire graph = Using up all the edge resources

$$\cfrac{\cfrac{\cfrac{\cfrac{{\color{red}Node(A)}\qquad {\color{red}Edge(A,B)}}{Node(B)} \qquad {\color{red}Edge(B,A)}}{Node(A)} \qquad {\color{red}Edge(A,C)}}{Node(C)} \qquad {\color{red}...}}{Node(..)}$$

In linear logic the assumption is the resources in red is by default ephemeral (can only be used once).  
We will underline the resources if it is a reusable resource which is basically typical ND if we underlined all resources.

---


$$u: A \vdash A$$


#### Simultaneous Conjunction

A and B are both true in the same state.

Intro  

$$\cfrac{\Delta \vdash A \qquad \Delta' \vdash B}{\Delta, \Delta' \vdash A \otimes B}$$ 


* Given: Resource $\Delta$ can produce A
* 

Elim  

$$\cfrac{\cfrac{\Delta ...}{A \otimes B} \qquad \cfrac{[u:A, w:B]}{\cfrac{\Delta'...}{C}}}{C}$$


#### Alternative Conjunction

Intro