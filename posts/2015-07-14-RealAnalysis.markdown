---
title: Real Analysis
tags: mathcs, puremath
---

* $\mathbb{N}$ : Naturals
* $\mathbb{Z}$ : Integers, Naturals + Negatives 
  * -3,-2,-1,0,1...
* $\mathbb{Q}$ : $\frac{p}{q}$ where p,q are Integers

##### Dedekind cuts

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


##### Cauchy Sequence

$\underset{n \rightarrow \infty}{lim}a_n=b$


##### Cauchy Condition



##### Inner product

Inner product on a vector space V = Operation on a pair of vectors in V

discriminant proof of Cauchy-Schwarz Inequality valid for all inner products on a real vector space.

All inner products define a norm but not all norms are inner products.

---

### Topology

##### Metric Space

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

    

##### Limit aka Limit point

* Given Metric Space $(M,d)$
* $S \subseteq M$
* p is a limit of S iff $\exists p_n :: sequence$ s.t. $p_n converges to p$
    