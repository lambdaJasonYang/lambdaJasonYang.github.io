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

``` python
np.linalg.inv(x) 
```
   
An matrix $$ A_{m \times n} :: \mathbb{R}^{n} \rightarrow \mathbb{R}^{m} $$ 

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
$$ \{ \begin{bmatrix}1 \\0 \end{bmatrix}, \begin{bmatrix}0 \\1 \end{bmatrix} \}\qquad Canonical\ Basis$$
$$\{ \begin{bmatrix}1 \\0 \end{bmatrix}, \begin{bmatrix}1 \\1 \end{bmatrix} \}\qquad Another\ Basis$$


#### Common misunderstanding with linear (in)dependence:  

Despite the fact that one can envision $\begin{bmatrix}1 \\0 \end{bmatrix}$ as being a component of $\begin{bmatrix}1 \\1 \end{bmatrix}$ , these two basis vectors are independent.  

Analogy: 

* $x$ and $x + bi$ are independent 
* $x$ is the real component but without the imaginary component $bi$, $x + bi$ cannot be built 

#### Change of Basis
![](\images\mathML\basis.png)

  
Let $A :: V \rightarrow W$  
$B, \widetilde{B} \in basis(V)$  
$C, \widetilde{C} \in basis(W)$  
$S :: \widetilde{B} \rightarrow B$  
$T :: \widetilde{C} \rightarrow C$  

Goal is to design a change of basis for the matrix $A$ which we define as  
$\widetilde{A} :: \widetilde{B} \rightarrow \widetilde{C}$ 
  
Predefined Theorem: Each vector of one basis can be defined as a linear combination of another basis.
Using Thm we can design linear combinations:




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

# Geometric intrepetation
Norm on a vector space := a function from vectorspace to reals

# Matrix Decomposition

Triangular matrix
 * Determinant = product diagonal

# Continuous optimization

* Continuous Optimization
  * Unconstrained optimization - gradient descent
  * Constrained optimization
    * Lagrange multipliers
      * Convex optimization and duality
        * Linear programming
        * Convex conjugate
        * Quadratic programming


# Linear regression