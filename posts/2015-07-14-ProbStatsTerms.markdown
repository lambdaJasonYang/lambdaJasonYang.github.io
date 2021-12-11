---
title: Probability and Stats Terminology
tags: mathcs, appliedmath, physics
toc: y
---

[Probability playground](http://www.acsu.buffalo.edu/~adamcunn/probability/probability.html)

# Terms

* Probability space is $(\Omega , \mathcal{F} , P)$
* $X: \Omega \rightarrow \mathbb{R}^n$
* $\Omega$ is "Sample Space" which is actually the Statistical Population.
* $\mathcal{F}$ is "Event Space" which is actually the Statistical random samples.`````

Stats

* Population is reality
* Sample is a subset of population
* Statistics always use sample to estimate or model population. 
  * Aka use Subset of Population to estimate population.
* $X~N(0,1)$ means random variable $X$ has the normal distribution of mean 0 and variance of 1
  * $X$ is a random variable means we choose a subset aka sample $X$ from a population. 
    * This subset can take on many different RANDOM VARying combinations of values aka "random variable".
  * Wiki: Random Variable is any function that maps from the Sample Space to a Real number.
    * Sample Space is just the possible sample subsets of the population.

Population Mean

* $\mu = E(Y_i)$

Sample Mean

* $\bar{x}~N(\mu,\frac{\sigma^2}{n})$
* How does a sample mean have a distribution?
  * The sample mean is a RANDOM VARIABLE, not a constant, since it's value will differ depending on the subset of population sampled. This variability allows thie sample mean to have a distribution.
    * The meaning of a normally distributed sample means is  
    "the sampled mean has some probability of falling within some interval which follows a normal distribution"
  


Parameters

* $\mu$ mu is the mean  
* $\sigma$ sigma is std  (z-score = sigma)
* $\sigma^2$ sigma squared is variance

## Z-test T-test ANOVA
* z-test is closest to normal dist.  
* t-test is similar to z-test but takes into account degrees of freedom.  
* ANOVA-analysis of variance is basically t-test but with more than 2 populations

Tails 

* 2-tail test for Alt Hypothesis inequality
* 1-tail test for Alt Hypothesis gt or lt

Multiple Regression vs Multivariate regression

* Multiple regression means more than one independent variable 
  * Age, Weight, Height as predictors for one independent variable GPA
* Multivariate means more than one dependent variable




## Jaynes
