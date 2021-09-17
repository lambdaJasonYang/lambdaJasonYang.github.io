---
title: Euler, Complex and Imaginary
tags: mathcs
---


### Fundamental Theorem of Algebra 

* Complex polynomials(complex coefficients) can be factored in to n factors with n roots that may not be distinct.
* Since reals are subset of complex, implies typical real polynomials can always be factored resulting in real or complex roots.
* Complex numbers have a closure property in factoring
  
### Cart, Polar, PolarForm Coordinate Systems

| Syntax      | Description |
| ----------- | ----------- |
| Header      | Title       |
| Paragraph   | Text        |

$(x,y) \in Cart$  
$(r,\theta) \in Polar$  
$a+bi \in PolarForm$
$r(cos\theta + i sin\theta) = rcis\theta \in PolarForm$

$ATranslate :: Cart \leftrightarrow Polar$  
$ATranslate\ \ (x,y) = (r cos\theta,rsin\theta)\ where\ r=\sqrt{x^2 + y^2}$

* With cartesian (x,y) we can derive r which is length of hypotenuse and theta with inverse trig functions.
* With r and theta we can derive x and y.



$BTranslate :: PolarForm \leftrightarrow Polar$
$BTranslate\ \ r(cos\theta + i sin\theta) = (r cos\theta,rsin\theta)$



$(x,y) \leftrightarrow (r,\theta) \leftrightarrow r(cos\theta + i sin\theta) \leftrightarrow rcis\theta$

One can think of all of these as vectors.

### De Moivre Theorem  

$(r\ cis\ \alpha)(s\ cis\ \beta) = (r\cdot s)cis\ (\alpha + \beta)$

**multiplying vectors in PolarForm results in**  

 * $\cdot$ length of 2 vectors $(r\cdot s)$
 * $+$ angle of 2 vectors $(\alpha + \beta)$



if we assume r and s are 1 meaning we have unit vectors  
Multiplying unit length vectors simply results in rotations.

$(cos\theta + isin\theta)^{n}=cos\ n\theta + isin\ n\theta$  

short hand: $(cis \theta)^{n} = cis\ n\theta$



**Warning:**   

* Multiplying Cartesian $\neq$ Multiplying PolarForm  
* $(a,b)\times(c,d) \neq cis\alpha \times cis\beta$  
* One must first transform Cartesian to PolarForm to multiply them then convert back to Cartesian

---

$\underset{n\rightarrow\infty}{lim}(1+\frac{1}{n})^n = e$ 


---

### Functional identities



##### Substitution method

Logarithm satisfies
$\forall f:f(x \cdot y) = f(x) + f(y)$

* $\text{[let f be an arbitrary function]}$
  * $let\ y = 1$ [Substitution]
    * $f(x \cdot 1) = f(x) + f(1)$
    * $f(x) = f(x) + k$
* $\forall f:f(1) = 0$

Log(1) = 0

$f(1) = 0$ is true for any function f that satisfies the identity $f(x \cdot y) = f(x) + f(y)$

DO NOT mistake f as a proposition, $\xcancel{[y = 1] \Rightarrow f(1)=0}$  
**Functions exist in a higher context, almost like a global namespace**

This means substitution is a valid method(just as we substituted y for 1) to understanding the generalized function that satisfies the functional identity.

Analogy: 

* substituting variables to fill in the puzzle of how the function behaves.   
* **SUBSTITUTION is NOT ASSUMPTION** because Functions are **defined** .
  * There is no need to "discharge" a substitution unlike a proposition   
* We are teasing out the behavior of the function with each substitution


---


### Derivatives

Derivatives operate on functions

$D(f(x)) + D(g(x)) = D(f+g)(x)$  
$c \cdot D(f)(x) = D(c \cdot f)(x) ,\ given\ constant\  c\in \mathbb{R}$  
Derivative operator is a linear operator


Functional identity for trig functions
$f(x+y) = f(x)g(y) + f(y)g(x)$
