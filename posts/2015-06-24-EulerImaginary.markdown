---
title: Euler, Complex and Imaginary
tags: mathcs
---


### Fundamental Theorem of Algebra 

* Complex polynomials(complex coefficients) can be factored in to n factors with n roots that may not be distinct.
* Since reals are subset of complex, implies typical real polynomials can always be factored resulting in real or complex roots.
* Complex numbers have a closure property in factoring
  
### Coordinate systems

Everything in the table is equal

| cart | complex | geometric cart |geocomplex| polar |
| ---- | ---- | ---- | ---- | ---- | 
| (x,y) | x+yi | (rcos$\theta$,rsin$\theta$) | $r(cos\theta + i sin\theta)$ | ((r, $\theta$)) |

Either find $x,y$ or find $r,\theta$  


* Find $r,\theta$
  * $r=\sqrt{x^2 + y^2}$
  * $\theta=tan^{-1}\frac{y}{x}$
* Find $x,y$
  * $x=rcos\theta$
  * $y=rsin\theta$






One can think of all of these as vectors.

Shorthand notation $rcis\theta = ((r,\theta))$

### De Moivre Theorem  

$(r\ cis\ \alpha)(s\ cis\ \beta) = (r\cdot s)cis\ (\alpha + \beta)$

We can derive this by converting polar to geocomplex then multiplying.

**multiplying vectors in polar results in**  

 * $\cdot$ length of 2 vectors $(r\cdot s)$
 * $+$ angle of 2 vectors $(\alpha + \beta)$



if we assume r and s are 1 meaning we have unit vectors  
**Multiplying unit length vectors simply results in rotations.**

$(cos\theta + isin\theta)^{n}=cos\ n\theta + isin\ n\theta$  

short hand: $(cis \theta)^{n} = cis\ n\theta$



**Warning:**   

* DO NOT multiply elements with a Coordinate system type
  * There isn't even an operation defined for Multiplying by cartesian or polar coordinate systems !!
* BAD $(a,b)\cdot(c,d)$
* BAD $((r,\theta)) \cdot ((s,\delta))$  
* Look at the above table, translate Coordinate to Complex form
  * {cart,polar,geometric cart} $\rightarrow$ {complex,geocomplex}
* GOOD  $(r\ cis\ \alpha)(s\ cis\ \beta) = r(cos \alpha + i sin \alpha) \cdot s(cos\beta + i sin \beta) = ...$

---

$\underset{n\rightarrow\infty}{lim}(1+\frac{1}{n})^n = e$ 


---

### Functional identities

Imagine if we had a functional identity
$$ \forall a: \forall f : f(a) = 1 $$

The only function that satisfies this is $f(x) = 1$

Note $\{\forall a: \forall f : f(a) = 1\} \neq \{f(x) = 1\}$

One is a functional identity and the other is a single function.


##### Substitution method

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



---


### Derivatives

Derivatives operate on functions

$D(f(x)) + D(g(x)) = D(f+g)(x)$  
$c \cdot D(f)(x) = D(c \cdot f)(x) ,\ given\ constant\  c\in \mathbb{R}$  
Derivative operator is a linear operator


Functional identity for trig functions
$f(x+y) = f(x)g(y) + f(y)g(x)$
