---
title: Abstract Linear Algebra
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

Given some vector space $S$, we choose some subspaces $\{X,Y,Z...\}$

1. None of these subspaces share any element but the null vector
2. there is no combination of $\{Y,Z...\}$ that can form an element of $X$.

take an element from each subspace in $\{X,Y,Z...\}$ then add them.  
$$\oplus \{X,Y,Z...\} = \{x + y + ... | x \in X, y \in Y ...\}$$ 

##### Direct Sum of matrices

Given that $A$ and $B$ are matrices themselves.  
Note that $0$ stands for zero matrices of varying sizes.  
Block matrix notation of direct sum:  

$$A \oplus B = \begin{bmatrix}
A & 0 \\
0 & B 
\end{bmatrix}$$

$$B \oplus A = \begin{bmatrix}
B & 0 \\
0 & A 
\end{bmatrix}$$

$$ A \oplus B \cong B \oplus A $$

* Direct sum is commutative up to isomorphism

##### Example

$\{(x,0) | x\in \mathbb{R}\} \oplus \{(0,y) | x\in \mathbb{R}\}=\mathbb{R}^2$ or written as   
$(\mathbb{R},0) \oplus (0,\mathbb{R})$   
but some might even do notational abuse and just write   
$\mathbb{R} \oplus \mathbb{R}$




