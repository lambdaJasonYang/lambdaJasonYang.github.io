---
title: Quick Stats Part 1
tags: mathcs, appliedmath
---

Keypoints:

* Test = Proof by Contradiction
* Null = Assumption Of Equality(=)
* Failing a Proof by Contradiction Doesn't Imply our Assumption is True
* Failing a test with high p-value Doesn't Imply our Null(=) is True


###  Tests are like proof by contradiction.  

1. Goal: Prove two groups are not equal. $T_1 \neq T_2$  

2. Null hypothesis is our assumption.
[$T_1 = T_2$]

3. If p-value is less than alpha, we have a proof by contradiction.  

4. Discharge assumption $[T_1 = T_2]$ (Reject the Null)   
thus proving $T_1 \neq T_2$ 


$$\frac{\frac{[T_1 = T_2]^{null}}{\frac{..p < 0.05..}{\bot}}}{T_1 \neq T_2}$$

alpha or 0.05 is Type 2 error.   

p-value is probability of seeing an unequal outcome given that something is equal.

* p-value is a measure of "if the null hypothesis is true,how surprised are you at the results"

$Prob(T_1 \neq T_2 | T_1 = T_2) := 0.05$
 
* Low p-value means "\


P-value does not show you probability of two groups being equal or a coin being fair.
$\xcancel{Prob(T_1 = T_2) = pvalue}$

