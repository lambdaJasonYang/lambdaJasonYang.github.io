---
title: Linear and Abstract Algebra
tags: mathcs, appliedmath
---

```plantuml
@startuml
frame "Abelian Group"{
    frame Group{
        frame Monoid {
            frame Semigroup {
            object "Set M" as Set
            object "Binary Operation" as Bin
            }
            object "identity element" as One
            One --> Set
        }
        object "inverse elements" as Inv
        Inv --> Set
    }
    object "Communatitive" as Comm
    Comm --> Bin
}

@endumlml
```

```bash
#print vector_space
vector_space (R: Type u) (M: Type v) [field R] [add_comm_group M] := semimodule R M
```

vector space is a 

* semi-module over a
  * field R 
  * additive communitative group M
```plantuml

```

#### Direct Sum

> Find two unrelated spaces in your universe and slam them together 

Given some vector space S, we have subspaces {X,Y,Z...}

1. None of these subspaces share any element but the null vector
2. there is no combination of {Y,Z...} that can form an element of X.

Inner Direct Sum is the set of possible sums that can be formed from taking one element from each of the subspaces {X,Y,Z...}  


$\{(x,0) | x\in \mathbb{R}\} \oplus \{(0,y) | x\in \mathbb{R}\}=\mathbb{R}^2$ or written as   
$(\mathbb{R},0) \oplus (0,\mathbb{R})$ but some might even do notational abuse and just write 
 $\mathbb{R} \oplus \mathbb{R}$