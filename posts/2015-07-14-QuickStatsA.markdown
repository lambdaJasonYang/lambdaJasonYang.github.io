---
title: Quick Stats Part 1
tags: mathcs, appliedmath, AI, stats
---

> All men are [EQUAL] in innocence until proven otherwise

Keypoints:

* Test = Proof by Contradiction
* Null = Assumption Of Equality(=)
* Failing a Proof by Contradiction Doesn't Imply our Assumption is True
* Failing a test with high p-value Doesn't Imply our Null(=) is True


###  Tests are like proof by contradiction.  


1. Null hypothesis is our assumption. $[T_1 = T_2]^{null}$

2. If p-value is less than alpha, we have a proof by contradiction.  

3. Discharge assumption $[T_1 = T_2]^{null}$ (Reject the Null)   
thus proving $T_1 \neq T_2$ 


$$\cfrac{\cfrac{[T_1 = T_2]^{null}}{\cfrac{..p < 0.05..}{\bot}}}{T_1 \neq T_2}$$

alpha or 0.05 is Type 2 error.   


* p-value is a measure of "if we assumed null hypothesis is true,how surprised are you at your empirical observation[data set]"



P-value does not show you probability of two groups being equal.
$\xcancel{pvalue = P(T_1 = T_2)}$


$$pvalue = P(D|H)\ \text{where H is null hypothesis}$$

* This means the p-value is the probability we see our data set appear in reality given a true null-hypothesis.
  * Example: T-test on 2 groups, Boy and Girl, on chocolate eaten. We empirically collect data on mean and stdev, our T-test shows p-value of 0.024.
    * Given our priori model of reality where that girl and boys eat same amount of chocolate, we find there is 2.4% chance that we see our data set happen in this model of reality. 
      * Either we got really lucky or our priori(null hypothesis) is wrong.
  * Example2: T-test on feathers vs rocks. We empirically collect data on acceleration mean and stdev. Our T-test shows p-value of 0.95.
    * Given our priori model of reality [Gravity], we have a 95% chance of seeing this happen. 
      * BUT NOTICE, OUR feathers vs rocks experiment does not prove gravity(null hypothesis) $P(H|D)$  
      $P(D|H) \neq P(H|D)$
    
---

> Given our priori model of reality where X and Y are Equal (Null hyp),  
We find there is a {p-value} chance that we see our empirically observed dataset obtained from our experiment.

$$P(H|D) = \frac{P(H)P(D|H)}{P(H)P(D|H)+P(\lnot H)P(D|\lnot H)}$$



### Chi-Squared test

INPUT:
function that maps alphabet to frequency, $f_1, f_2$  
Notice both the Domain(Alphabet) and CoDomain(Naturals) are discrete aka Countable.   

OUTPUT:
$f_1 \overset{?}{=} f_2$

Example:  
Chi-Square test on Cesar Cipher,

ANALYZE:  
$ChiSquared = 0 \Rightarrow f_1 = f_2$
$ChiSquared \propto $ 