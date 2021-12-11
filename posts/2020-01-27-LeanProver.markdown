---
title: Lean
tags: mathcs, puremath ,categorytheory
toc: y
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

## Example

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

# Types

if ` p : Prop `{.hs}  then
`x:p`{.hs} is a proof of proposition `p : Prop`{.hs}.
if `y:p`{.hs} is also a proof then **proof irrelevance** means `x:p`{.hs} and `y:p`{.hs} are indistinguishable.  


* Props, terms 
  * Given $P:Prop$ , the term $h:P$ is a proof of $P$
  * Implication $P \rightarrow Q : Prop$ is the function type
  * Given $h1: P \rightarrow Q$ , $h2:P$ conclude $h1\ h2: Q$
* Quantifier and Dependent Function type
  * Given $P:X \rightarrow Prop$ represents $\forall( x: X). P\ x: Prop$

# Numbers

Proofs of numbers typically use type coercion.

```bash
norm_num
```

# Inductive type

```bash
inductive mynat
| zero : mynat
| succ (n : mynat) : mynat
```

# Definitional Equality and Reduction
Given  
  : $A \overset{reduces}{\rightarrow} X$  
  : $B \overset{reduces}{\rightarrow} X$  

Implies  
  : $A \underset{def}{\equiv} B$
   
| Conversion | Definition | Example |
| --- | --- | --- |
| alpha | renaming | $\lambda x. P x \equiv \lambda y. P y$  |
| beta | computing |  $(\lambda x. x + 2) 3 \equiv 3 + 2$ |




# Dependent type


## Inspiring the dependent type

`cons 2 [3,4] : List Nat`{.hs}  
`cons : Nat -> List Nat -> List Nat`{.hs}  

What if we wanted a polymorphic `cons`{.hs}?  
Give first argument as the Type  
`cons Nat 2 [3,4] : Type -> T -> List T -> List T`{.hs}   

* **Nat** : Type
* 2 : **T**
* [3,4] : List **T**

Notice `T`{.hs} = `Nat`{.hs}  

We need `T`{.hs} in Type-Space to relate to `Nat`{.hs} in Term-Space. How?    
Abstract away the term `Nat`{.hs} to a **bound term variable** `Π T`{.hs}   
`Type -> T -> List T -> List T`{.hs} =>  
` Π T : Type -> T -> List T -> List T`{.hs}



```bash
namespace hidden

universe u

constant list   : Type u → Type u

constant cons   : Π α : Type u, α → list α → list α
constant nil    : Π α : Type u, list α
constant head   : Π α : Type u, list α → α
constant tail   : Π α : Type u, list α → list α
constant append : Π α : Type u, list α → list α → list α

#check cons α a (nil α)

end hidden
```
Observe how we can use Dependent Product Type for polymorphism.

$$\prod_{x: \alpha} \beta = \prod_{x: \alpha} (x \rightarrow list\ x \rightarrow list\ x)$$

> Pi-type or dependent product type behaves like lambda in the type space

**β is an EXPRESSION that can be expanded.**  
**β may or may not include bound variable x in it's expression**

`cons :: Π x : α, β `{.hs}   
What is the type of `cons int`{.hs}?  
We apply `(Π x : α, β) `{.hs} to `int` in the type space.  
`cons int :: (Π x : α, β)(int)`{.hs}

1. `(Π x : α, β)(int) `{.hs}
2. Beta-reduction: `β[int/x]`{.hs}  
3. `β`{.hs} expanded is `x -> list x -> list x`{.hs}  
4. `β[int/x]`{.hs} is `(x -> list x -> list x)[int/x]`{.hs}  
5. replace x with int resulting in  
  `(int -> list int -> list int)`{.hs}  

`cons int :: (Π x : α, β)(int)`{.hs} is  
`cons int :: int -> list int -> list int`{.hs}  

Example of a list is `cons int 3 (nil int)`{.hs}  

Since `cons`{.hs} and `nil`{.hs} are dependent types, they must take an extra parameter [in this case] `int`{.hs}  

`f :: Π x : α, β`{.hs} =  `f x :: β[x]`{.hs} where `x :: α`{.hs}

### function type is just a Pi-type 

> when `β`{.hs} expanded expression does not include   
bound variable `Π x : α`{.hs} then  
`β[x]`{.hs} = `β`{.hs}  

example: 
`f :: Π x : α, β`{.hs} = `f x :: β[x]`{.hs} = `f x :: β`{.hs} = `f :: α -> β`{.hs}



example `β` is `bool`, `α` is `nat`  
Notice `bool` is an expression that does not include any bound variable `x:nat`

* `even :: Π x : α, β`{.hs} 
* `even :: Π x : nat, bool`{.hs}
* `even :: nat -> bool`{.hs}


## Sigma Type

```hs
def f (α : Type) (β : α → Type) (a : α) (b : β a) : (a : α) × β a :=
  ⟨a, b⟩
def h1 (x : Nat) : Nat :=
  (f Type (fun p => p) Nat x).2

-- (α : Type) is (Type : Type)
-- β is (fun p => p : Type -> Type)
-- (a : α) is (Nat : Type)
-- (b : β a) is (x : (fun p => p)(Nat)) which is (x : Nat)
```

How to look at `f`? Extract the types  
`Type -> (α → Type) -> α -> β a -> ((a : α) × β a)`{.hs}
