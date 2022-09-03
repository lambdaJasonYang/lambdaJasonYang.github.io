---
title: Real Analysis
tags: mathcs, puremath
---

* $\mathbb{N}$ : Naturals
* $\mathbb{Z}$ : Integers, Naturals + Negatives 
  * -3,-2,-1.5,0,0.5,1...
  * One can think of integers as equivalence classes or sets
  * 3/2 = {3/2,6/4,9/6,...}
  * This allows to use the min function
* $\mathbb{Q}$ : $\frac{p}{q}$ where p,q are Integers

# Fields 

Reals is a set with multiplication and addition satisfying the below:

* $(\mathbb{R}, +, 0)$ is an abelian group
* $(\mathbb{R}/\{0\}, \cdot, 1)$ is an abelian group
* Distributive law
* $\leq$ is  total order compatible with $+, \cdot$
* Every cauchy sequence is a convergent sequence

## Absolute value

$$ \lvert x \cdot y \rvert = \lvert x \rvert \cdot \lvert y \rvert $$
$$ \lvert x + y \rvert \leq \lvert x \rvert + \lvert y \rvert $$


# Dedekind cuts

* `Dedekind_cuts := (A,B) :: (Partition(Q),Partition(Q))`{.haskell}
  * $A,B \subseteq \mathbb{Q},\ A,B \neq \emptyset$
  * $(A,B) = 2\ partitions\ of\ Q$ 
    * $A \cup B = \mathbb{Q}$
    * $A \cap B \neq \emptyset$
  * $a \in A \land b \in B \Rightarrow a \lt b$
    * Every element of A is less than B
  * $A$ has no largest element

A real number is a Dedekind cut.  
A real number is represented by a partition of $\mathbb{Q}$.


# Cauchy Sequence

$\underset{n \rightarrow \infty}{lim}a_n=b$


# Cauchy Condition



# Inner product

Inner product on a vector space V = Operation on a pair of vectors in V

discriminant proof of Cauchy-Schwarz Inequality valid for all inner products on a real vector space.

All inner products define a norm but not all norms are inner products.

---

# Topology

## Metric Space

`Metric_Space := (M,dist) :: (Set,binary_operation)`{.haskell}


Fields of knowledge as Metric Spaces (M,d)

* Metric Space of Proofs in Math  
$(M,d) := (math,dist_{math})$
* Metric Space of Reactions in Chemistry  
$(M,d) := (chem,dist_{chem})$

Mapping metric spaces of knowledge AKA analogies  

* Analogies as a mapping between Metric Spaces of Knowledge
  * "Reagents is-to Reaction" as "Antecedents is-to Theorem"
  * $dist_{chem}(reagents,reaction) \approx dist_{math}(antecedents,theorem)$

    

## Limit aka Limit point

* Given Metric Space $(M,d)$
* $S \subseteq M$
* p is a limit of S iff $\exists p_n :: sequence$ s.t. $p_n converges to p$
    


# brightside

## 3

* A sequence is **bounded** if exist some number that bounds the sequence
* Convergent sequences implies bounded sequences but not otherway around
  * A bounded sequence can rapidly oscillate meaning it is nonconvergent



# Riemann vs Lesbesgue integral

## Riemann

* Area under the curve by summing infinitely small deltas.

Problems:

* Cannot expand to higher dimensions
* Dependent on continuity

$$lim_{n \to \infty} \int^{b}_{a} f_n(x)dx \overset{?}{=} \int^{b}_{a} lim_{n \to \infty} f_n(x)dx$$

The above equality is contingent on uniform converge of the function f, for Riemann integrals BUT  
we know empirically that the equality is true for far weaker constraints.  

## Lesbesgue

* Instead of delta on x-axis, we choose deltas on y-axis. 
  * visually Horizontal slices or rectangles
* Since we choose delta on y-axis we multiply it by the preImage.


```bash
            Vector Space
                 |
           Normed Space
            /         \
Inner Product Space   Banach Space
            \         /               
          Hilbert Space
```