---
title: Math for ML Notes
tags: tech
---

Notes on Math for ML



Gaussian transformation are linear transformations that result in row-echelon form

* pivots - leading coefficient of a row
  * pivot is called basic variable 
  * non-pivot variable is called free variable

* row-echelon form
  * reduced row-echelon have pivots=1, and pivot is the only nonzero entry

* Rank - number of matching independent rows with independent columns

Given a vectorspace V, a set of vectors Q that can build any element in this vector space is the "generating set"
V = span(Q)
This says Q spans V.

Note there can be multiple generating sets that span V that hold dependent or independent vectors.
We call the minimal such set, the basis and it is easy to see the basis is linearly independent.
$$\widetilde{A}$$ 
$\widetilde{A}$ is the minimal set that spans V
$\widetilde{A}$ is the maximal set that is linearly independent.
  
Note minimal and maximal are not unique.
The basis is not unique.
There can be multiple basis but they will all have the same number of elements.

kernel or null space of a matrix or linear transformation is the preimage or set of vectors that hits the zero vector.


Matrices are called linear mapping or vectorspace homomorphism or linear transformation
Matrices are linear maps which can be classified as

* Isomorphism V → W ,bijective
* Endomorphism  V → V
* Automorphism V → V, bijective
* identity Automorphism x ↦ x :: V → V


Finite-dim vector spaces V and W are is
Basis Change


Affine subspace or Linear manifold