---
title: Sequents and Gentzen Cut
tags: mathcs
---
a and b are Predicates.
$$a \vDash b$$  
a satisfies b  
$x :: U$
$a, b :: U \rightarrow Bool$
$\{x | a(x)\} \subset \{x | b(x)\}$$

$\Gamma :: \{Prop\}$  
$\Delta :: \{Prop\}$  
$\Gamma \Delta$ are the set of propositions in this universe.


## Gentzen Cut

Gentzen Cut is simply a syntatic trick

$$
\frac{a \vDash b \ \ \ \ b \vDash c}{a \vDash c}
$$

$$
\frac{ {\color{blue}\Gamma , a} \vDash b , {\color{red}\Delta} \ \ \ \ {\color{blue}\Gamma} , b \vDash{\color{red} c , \Delta}}{{\color{blue}\Gamma , a} \vDash  {\color{red}c , \Delta}}
$$

We expand 

$$
\frac{ {\color{blue}\Gamma , a} \vDash b , {\color{red}c , \Delta} \ \ \ \ {\color{blue}\Gamma, a} , b \vDash{\color{red} c , \Delta}}{{\color{blue}\Gamma , a} \vDash  {\color{red}c , \Delta}}
$$
Absorbing a into Gamma  
Absorbing c into Delta  
$$
\frac{\Gamma' \vDash b , \Delta' \ \ \ \ \Gamma' , b \vDash \Delta'}{\Gamma' \vDash  \Delta'}
$$

Why?
Remember 
$AND \vdash OR$
