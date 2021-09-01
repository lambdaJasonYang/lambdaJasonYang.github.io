---
title: Euler, Complex and Imaginary
tags: mathcs
---


### Fundamental Theorem of Algebra 

* Complex polynomials(complex coefficients) can be factored in to n factors with n roots that may not be distinct.
* Since reals are subset of complex, implies typical real polynomials can always be factored resulting in real or complex roots.
* Complex numbers have a closure property in factoring
  
### Cart, Polar, PolarForm Coordinate Systems

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

$(r\ cis\ \alpha)(s\ cis\ \beta) = (r\times s)cis\ (\alpha + \beta)$

**multiplying vectors in PolarForm results in**  

 * $\times$ length of 2 vectors $(r\times s)$
 * $+$ angle of 2 vectors $(\alpha + \beta)$



if we assume r and s are 1 meaning we have unit vectors  
Multiplying unit length vectors simply results in rotations.

$(cos\theta + isin\theta)^{n}=cos\ n\theta + isin\ n\theta$  

short hand: $(cis \theta)^{n} = cis\ n\theta$



**Warning:**   

* Multiplying Cartesian $\neq$ Multiplying PolarForm  
* $(a,b)\times(c,d) \neq cis\alpha \times cis\beta$  
* One must first transform Cartesian to PolarForm to multiply them then convert back to Cartesian