---
title: Quick Stats Part 1
tags: mathcs, appliedmath, AI, stats
---

Keypoints:

* Test = Proof by Contradiction
* Null = Assumption Of Equality(=)
* Failing a Proof by Contradiction Doesn't Imply our Assumption is True
* Failing a test with high p-value Doesn't Imply our Null(=) is True


###  Tests are like proof by contradiction.  

2. Null hypothesis is our assumption. $[T_1 = T_2]^{null}$

3. If p-value is less than alpha, we have a proof by contradiction.  

4. Discharge assumption $[T_1 = T_2]^{null}$ (Reject the Null)   
thus proving $T_1 \neq T_2$ 


$$\cfrac{\cfrac{[T_1 = T_2]^{null}}{\cfrac{..p < 0.05..}{\bot}}}{T_1 \neq T_2}$$

alpha or 0.05 is Type 2 error.   

p-value is probability of seeing an unequal outcome given that something is equal.

* p-value is a measure of "if the null hypothesis is true,how surprised are you at the results"

$Prob(T_1 \neq T_2 | T_1 = T_2) := 0.05$
 
* Low p-value means "\


P-value does not show you probability of two groups being equal or a coin being fair.
$\xcancel{Prob(T_1 = T_2) = pvalue}$


### Chi-Squared test

INPUT:
function that maps alphabet to frequency, $f_1, f_2$  
Notice the Domain(Alphabet) and CoDomain(Naturals) are discrete variables.   

OUTPUT:
$f_1 \overset{?}{=} f_2$

Example:  
Chi-Square test on Cesar Cipher,

ANALYZE:
$Chi-Squared = 0 \Rightarrow f_1 = f_2$
$Chi-Squared  