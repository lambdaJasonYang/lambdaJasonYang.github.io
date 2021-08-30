---
title: Gentzen
tags: mathcs
---

$$x \vDash y$$  
x satisfies y  
$x :: Prop$  
$\Gamma :: \{Prop\}$  
$\Delta :: \{Prop\}$  
$\Gamma \Delta$ are the set of propositions in this universe.


## Gentzen Cut

Gentzen Cut is simply a syntatic trick

$$
\frac{a \vDash b \ \ \ \ b \vDash c}{a \vDash c}
$$

$$
\frac{\Gamma , a \vDash b , \Delta \ \ \ \ \Gamma , b \vDash c , \Delta}{\Gamma , a \vDash  c , \Delta}
$$

Absorbing a into Gamma  
Absorbing b into Delta  
$$
\frac{\Gamma \vDash b , \Delta \ \ \ \ \Gamma , b \vDash \Delta}{\Gamma \vDash  \Delta}
$$

