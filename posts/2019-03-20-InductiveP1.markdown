---
title: Simple Inductive proofs
tags: mathcs
---


Problem: Cutting an area into N regions  

AM-GM Theorem  
$$ (x_1 x_2 .. x_n)^{1/n} \leq \frac{x_1 + x_2 + .. x_n}{n} $$  

### Upward induction step 1  


Base step: n = 1, obvious  
Base step: n = 2, obvious  


$(P(k) \rightarrow P(2k) )$:  

for $k \geq 2$ IH:  
$$(x_1 x_2 .. x_k)^{1/k} \leq \frac{x_1 + x_2 + .. x_k}{k}$$

Prove P(2k) is true:
$$ (x_1 x_2 .. x_{2k})^{1/2k} \leq \frac{x_1 + x_2 + .. x_{2k} }{2k} $$

Thoughts:

* Attempt to shift complexity towards one side for IH
  * IH: $(x_1 x_2 .. x_k) \leq (\frac{x_1 + x_2 + .. x_k}{k})^{k}$
* Separate problem to subproblems we call subpartA, subpartB
  * $(x_1 x_2 .. x_{2k})$ to  subpartA: $(x_1 x_2 .. x_k) , subpartB: (x_{k+1} x_{k+2} .. x_{2k})$ 
* Technique - apply induction to the subproblems
  * IH(subpart) IH(subpartB)
* Technique - Algebraic and Inequality manipulation
  * 
### Downward induction step 2  

Thoughts:

$A= \frac{a_1 + a_2 + .. + a_{m-1}}{m-1}$