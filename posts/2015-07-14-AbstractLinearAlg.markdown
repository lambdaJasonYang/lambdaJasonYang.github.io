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

