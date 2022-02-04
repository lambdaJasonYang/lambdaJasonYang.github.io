---
title: Logical Foundations notes
tags: mathcs, logic
toc: y
---

# Polymorphism
 

in C++ we have `vector<int>` as the type for a list of int. For a polymorphic list, we have `vector<T>` which is really a dependent type `Π( T : Type*) ->  vector T`

```{.hs filename=coq}
Inductive natlist : Type :=
  | nil
  | cons (n : nat) (l : natlist).

(** For example, here is a three-element list: *)

Definition mylist := cons 1 (cons 2 (cons 3 nil)).

Check mylist.
```

$$\cfrac{}{\vdash nil : natlist} \qquad \cfrac{\vdash n: nat \qquad \vdash l:natlist}{\vdash cons\ n\ l : natlist}$$

# inductive Type, Type formation, Term intro

* Type formation and Term introduction rules are inside the `Inductive` type keyword
$$\cfrac{}{\vdash False : Prop} \tag{type formation}$$
$$\text{There is no term intro for False} \tag{term intro}$$
```{.hs filename=False.coq}
Inductive False : Prop.
```

$$\cfrac{}{\vdash True : Prop} \tag{type formation}$$
$$\cfrac{}{\vdash top : True} \tag{term intro}$$

```{.hs filename=True.coq}
Inductive True : Prop :=
  | top : True.
```
$$\cfrac{\vdash P : Prop \qquad \vdash Q : Prop}{\vdash and\ P\ Q : Prop} \tag{type formation}$$

$$\cfrac{\vdash x : P \qquad \vdash y : Q}{\vdash conj\  x\ y\ : and\ P\ Q} \tag{term intro}$$


```{.hs filename=and.coq}
Inductive and (P Q : Prop) : Prop := --Type formation
  | conj : P -> Q -> and P Q. --Term introduction

Check conj True True Top Top.
```
* Notice we have to do `conj nat nat 2 4` NOT `Check conj 2 4.` because we actually defined a dependent type.
  * `conj :: Π(P : Prop) ->  Π(Q : Prop) -> P -> Q`

```{.hs filename=logic.lean}

inductive falseN : Prop --no intro rules for Bottom Type

inductive trueN : Prop --only 1 intro rule for Top
 | introT : trueN
--  ------
--    T

inductive andN (a b : Prop) : Prop 
 | introConj : a -> b -> andN
--   a  b
-- --------
--  a ∧ b
 
 inductive orN (a b : Prop) : Prop 
  | intro_left: a -> orN 
  | intro_right: b -> orN
--     a          b
--  --------   -------
--   a ∨ b     a ∨ b
``` 

```{.v filename=logic.v}

```

### SUM TYPE and PRODUCT TYPES
 use inductive keyword to create intro rules
 use def or function definitions to create elim rules

```hs
universes j k 



inductive ProdN (a : Type j) (b : Type k) 
| pair : a -> b -> ProdN

inductive Either (a : Type j) (b : Type k)
| inl : a -> Either 
| inr : b -> Either

def EitherElim :  (Either nat string) -> nat  
| (inl 4)  := 6 

open ProdN 
open Either 
#check pair nat char 
-- pair ℕ char : prodN Type Type


#check inl nat
--inl ℕ : Either Type ?M_1
#check@ inl nat 
--inl : Π {b : Type u_1}, ℕ → Either ℕ b
```
in haskell

```hs
data Either a b = Left a | Right b
Left nat :: Either nat M? 
```




## What is the meaning of a proposition

* To understand meaning we need to combine duality of Verificationist and Pragmatist
  * Verificationist tells us meaning is defined by how it is built. 
    * The meaning of a proposition is precisely the components or nodes that build it
    * Verificationist owns the Intro rules
  * Pragmatist tells us meaning is defined by how it is deconstructed.
    * The meaning of a proposition is precisely the things we can conclude from the proposition
    * Pragmatist owns the Elim rules 

### Harmony

<!--  -->
* Verificationist
  * intro then elim, then reduce the proof tree
    * the resulting proof tree lacks elim rules 
<!--  -->
* Pragmatist
  * elim then intro, then reduce the proof tree
    * the resulting proof tree lack intro rules





# Idiosyncrasy

* Coq will give an error for the below
```hs
Inductive and : Prop -> Prop -> Prop := 
  | introConj (a: Prop) (b:Prop) : and.
```
* We need to push back the props 
```hs
Inductive and (P Q : Prop) : Prop :=
  | introConj : P -> Q -> and P Q.
```