---
title: Inner Product and Norms
tags: mathcs, puremath, categorytheory
toc: y
---
# Inner Product is a Metric

 $$\langle x,y \rangle = d(x,y) = x^T y$$  
 Inner Product is a Metric function

$$\sqrt{\langle x,x} \rangle = \|x\| =d(x,0)$$
Norm is just an inner product against the origin with these constraints  

* $d(p+x,p+y) = d(x,y)$  
* $\|k\| \cdot d(x,y) = d(kx,ky)$



# Euclidean Norm(2-Norm)

$$\cdot \text{ is Norm Operation}$$

$$X \cdot Y = X^T Y$$

# Inner Product Space

Inner Product Space = Vector Space  
for all intents and purposes

Inner product space just means vector space with a defined inner product function.  

# Adjoint

`T :: V -> W`{.hs}  
`T* :: W -> V`{.hs}  

$$\langle Tv, w \rangle = \langle v, T^* w \rangle$$