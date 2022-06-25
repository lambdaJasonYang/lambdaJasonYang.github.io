---
title: Functional equations and why we need to verify solutions
tags: mathcs, mathbasics
---

# Substitution

* $f(x) = 2x$
* $f(2) = 4$
* $f(g(y)) = 4$
  * $g = \forall y, y \mapsto 2$

What does it **really** mean when we substituted $2$ for $x$?  
One way to look at it is we really substituted function $g(y)$ for $x$.  
function $g(y)$ maps all numbers to 2. Graphically this is a just a horizontal line on 2.   
We can tell substitutions aka $g$ function is not injective.  

# Why do we need to verify solutions?

Keypoint: Some equation simplifications are **non-invertible**

3 examples of 3 Types of equation reduction:  

1. $eq_1\overset{?}{=}eq_2 \overset{\square^2}{\Longrightarrow} eq_1^2\overset{?}{=}eq_2^2$
2. $eq_1\overset{?}{=}eq_2 \overset{\sqrt{\square}}{\Longleftarrow} \sqrt{eq_1}\overset{?}{=}\sqrt{eq_2}$
3. $eq_1\overset{?}{=}eq_2 \overset{+2}{\Longleftrightarrow} eq_1+2\overset{?}{=}eq_2+2$





# Functional identities

Imagine if we had a functional identity
$$ \forall a: \forall f : f(a) = 1 $$

The only function that satisfies this is $f(x) = 1$

Note $\{\forall a: \forall f : f(a) = 1\} \neq \{f(x) = 1\}$

One is a functional identity and the other is a single function.


## Substitution method

Logarithm satisfies
$\forall x,y : \forall f:f(x \cdot y) = f(x) + f(y)$

* $[assume\ \hat{a}]$
  * $[let\ \exists \bar{b}, \bar{b}= 1]$
    * $[assume\ \hat{f}]$
      * plug in $\hat{a} , \bar{b}, \hat{f}$ into functional identity
      * $\hat{f}(\hat{a} \cdot \bar{b}) = \hat{f}(\hat{a}) + \hat{f}(\bar{b})$ , apply substitution $\bar{b} = 1$
      * $\hat{f}(\hat{a} \cdot 1) = \hat{f}(\hat{a}) + \hat{f}(1)$
      * $\hat{f}(\hat{a}) = \hat{f}(\hat{a}) + \hat{f}(1)$
      * conclude $\hat{f}(1) = 0$
    * $\forall f:f(1) = 0$ discharge arbitrary $\hat{f}$
  * $b$ is no longer in context; Simply drop existential quantifier instead of discharging
* $\forall a : \forall f: f(1) = 0$ discharge arbitrary $\hat{a}$

Plug back our conclusion back into original functional identity for verification (not shown here).

Notice $Log(1) = 0$

$f(1) = 0$ is true for any function f that satisfies the identity $f(x \cdot y) = f(x) + f(y)$

