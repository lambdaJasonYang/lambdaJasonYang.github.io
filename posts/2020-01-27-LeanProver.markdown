---
title: Lean
tags: mathcs, categorytheory
---

| Logic | Intro | Elim |
| ---   | ---   | ---  |
| $\rightarrow$ | `intro`{.bash} `intros`{.bash} | `apply`{.bash}  `have h_3 :=  h_1 h_2`{.bash} |
| $\forall$ | `intro`{.bash} `intros`{.bash} | `apply`{.bash}  `specialize`{.bash} `have h_2 := h_1 t`{.bash} |
| $\exists$ | `use`{.bash}  | `cases`{.bash}  |
| $\lnot$ | `intro`{.bash} `intros`{.bash} | `apply`{.bash} `contradiction`{.bash}|  
| $\land$ | `split`{.bash} | `cases`{.bash} `h.1`{.bash} `h.2`{.bash} `h.left`{.bash} `h.right`{.bash}|
| $\leftrightarrow$ | `split`{.bash} |  `cases`{.bash} `h.1`{.bash} `h.2`{.bash} `h.mp`{.bash} `h.mpr`{.bash} `rw`{.bash} |
| $\lor$ | `left`{.bash} `right`{.bash} | `cases`{.bash} |
| $\bot$ | N/A | `contradiction`{.bash} `ex_falso`{.bash} |
| $\top$ | `trivial` | N/A |  

Classical logic `open_locale classical`{.bash} use `by_contradiction`{.bash} tactic

Most of the time, implication and universal quantifier are treated the same.

##### Example

```bash
expected type:
abcd: ℝ
h₁: a ≤ b
h₂: c ≤ d
⊢ ∀ {α : Type u_1} {a b c d : α} [_inst_1 : preorder α] 
[_inst_2 : has_add α] 
[_inst_3 : covariant_class α α (function.swap has_add.add) has_le.le] 
[_inst_4 : covariant_class α α has_add.add has_le.le]
, a ≤ b → c ≤ d → a + c ≤ b + d
```
Type 

For any type α that has a pre order. 

### Infinite primes 

