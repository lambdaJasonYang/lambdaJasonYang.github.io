---
title: Quick Stats Part 1
tags: mathcs, appliedmath, AI, stats
toc: y
---

> All men are [EQUAL] in innocence until proven otherwise

Keypoints:

* Test = Proof by Contradiction
* Null = Assumption Of Equality(=)
* Failing a Proof by Contradiction Doesn't Imply our Assumption is True
* Failing a test with high p-value Doesn't Imply our Null(=) is True


# Terms

$$P(X=\text{"it will rain today"})$$  
$X$ is the random variable  
$\mu$ mu is the mean  
$\sigma$ sigma is std  (z-score = sigma)
$\sigma^2$ sigma squared is variance. Variance is a measure of how dispersed the data is in absolute measure.


#  Tests are like proof by contradiction.  


1. Null hypothesis is our assumption. $[T_1 = T_2]^{null}$  

2. If p-value is less than alpha, we have a proof by contradiction.  

3. Discharge assumption $[T_1 = T_2]^{null}$ (Reject the Null)   
thus proving $T_1 \neq T_2$ 


$$\cfrac{\cfrac{[T_1 = T_2]^{null}}{\cfrac{..p < 0.05..}{\bot}}}{T_1 \neq T_2}$$

alpha or 0.05 is Type 2 error.   

Another interpretation: $[T_1 - T_2 = 0]^{null} \overset{?}{\Rightarrow} T_1 - T_2 \neq 0$  
This looks similar to the T-test for linear regression $[\beta = 0]^{null} \overset{?}{\Rightarrow} \beta \neq 0$

* p-value is a measure of "if we assumed null hypothesis is true,how surprised are you at your empirical observation[data set]"

## P-value  

t-score=2.8 p-value=0.006
```hs
A :: population
B :: population
x :: sample
y :: sample
```

Given 2 sets of populations A,B  
We perform a random sampling and observe sample subsets of population A which is x and population B which is y.

$x \subseteq A$  
$y \subseteq B$  

> Assuming that the population A and B are equal, random sampling of pairs (x,y) taken from populations A and B which is our observations, would result in a t-score that is at least 2.8 with a probability of 0.6% 

$$ P( tscore(x,y) \geq 2.8 | A = B) = 0.6% $$

note the t-score reflect difference between sample means: $\bar{x} - \bar{y}$

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



# Chi-Squared test

INPUT:
function that maps alphabet to frequency, $f_1, f_2$  
Notice both the Domain(Alphabet) and CoDomain(Naturals) are discrete aka Countable.   

OUTPUT:
$f_1 \overset{?}{=} f_2$

Example:  
Chi-Square test on Cesar Cipher,

ANALYZE:  
$ChiSquared = 0 \Rightarrow f_1 = f_2$
$ChiSquared \propto$

# F-test

# Z-test

$$ z-score = \sigma = stddev $$


# T-distribution

* T-dist is a probability dist like a normal-dist but bigger tails
  * infinite degree of freedom result in T-dist = normal-dist
* smaller degree of freedoms imply bigger tails
* Can be useful to model returns with fat fails

### degree of freedom

