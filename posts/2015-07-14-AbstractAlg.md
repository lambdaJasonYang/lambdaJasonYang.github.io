---
title: Abstract Algebra
tags: mathcs, appliedmath
toc: y
---

# Cayley Theorem

1. Lets define a group
  * $G = { a b c d e}$  
2. Pick an element of that group let's say $c$.  
Left multiply $G$ by $c$
  * $cG = { ca cb cc cd ce}$
  * $cG = { b e a c d}$ 
3. Notice the resulting multiplication just created a permutation of G.
  * $cG \in Perm(G)$
    * $Perm(G)$ = Set containing different permutations of G including group G itself
4. We can define the left multiplication of $G$ as a higher order function `f`
  * `f :: G -> {G} -> Perm(G)`
  * `f(c) = cG` for our specific example
  * function f takes a group element (in our example $c$) and our Group G and returns a permutation of G.
5. We can curry `f` to `lambda x, f(x) :: {G} -> Perm(G)`
  * In abstract algebra this means that Group $G4 is isomorphic to a subgroup $S_G$ which is termed the Permutation group.



