---
title: Logical Foundations notes
tags: mathcs, logic
toc: y
---

# Understanding Coq and Lean

## Lean variables vs constants

* variables mean free variables

```hs
variables P Q : Prop
```


## Inductive dependent types

* `inductive nat: Type` is a plain inductive type
* `inductive nat (k: Type) : Type` is a dependent inductive type `
* `inductive nat : Type -> Type` is same as above
<!--  -->
* whenever our `Inductive` associates with function type, it indicates we are using a dependent type
  * `Inductive binary_word1 (n:nat) : Type` means that `binary_word1` takes a `(n:nat)` and returns a new type. This means the canonical types we see in the wild is `binary_word1 1, binary_word1 2,...`  
  * `Inductive binary_word : nat -> Type` shows that `binary_word` has a function type meaning it is a dependent type.


```hs
--Lean

--Coq (2 approaches)
Inductive binary_word1 (n:nat) : Type 
  := bwc (l : list bool) (_ : length l = n).

Inductive binary_word : nat -> Type :=
  | bw_nil : binary_word 0
  | bw_cons : forall n, bool -> binary_word n -> binary_word (S n).

```

## Coq vs Lean

### Inductive type

* Type formation diff - Coq requires specifying output type, it is optional in Lean
  * Lean `inductive prod (A B : Type) : Type`
    * `inductive prod (A B : Type)` Lean can also omit the output type 
  * Coq `Inductive prod (A B : Type) : Type`
* Term introduction/ constructors - Lean makes the type implicit, Coq makes it explicit
  * Lean `mk 2 4 :: prod nat nat` 
  * Coq `mk nat nat 2 4 :: prod nat nat`
  

```hs
namespace hidden 
inductive prod (A B : Type) : Type
| mk : A → B → prod
open prod
#check mk 2 4 
-- mk 2 4 :: prod nat nat

inductive prod (A B : Type) --Output type omitted
| mk : A → B → prod
end hidden
```




```hs
                          -- DIF--
Inductive prod (A B: Type) : Type :=
  | mk (x:A) (y:B) .
Check mk nat nat 2 4. --notice how this diff from Lean's mk

--we can also add typing to our term intro/constructor
Inductive prod (A B:Type) : Type :=
  | pair (x:A) (y:B) : A -> B -> prod A B.
```

A more closer lean analogy to coq's prod is below

```hs

inductive prod : Type -> Type -> Type 1
| mk : Π (α β : Type), α → β → prod α β

#check mk nat nat 2 4 
--mk nat nat 2 4 :: prod ℕ ℕ
#check mk
--mk :: Π (α β : Type), α → β → prod α β
```
which we can also mimic in coq

```hs
Inductive prod :  Type -> Type -> Type :=
  | mk : forall A B :Type , A -> B -> prod A B .

Check pair nat nat 2 4.
(*  pair nat nat 2 4 :: prod nat nat *)

Check mk.
(* mk :: forall A B : Type, A -> B -> prod A B *)
```

### Implicit 

* Notice how with `(A B : Type)` the result of `mk 2 4 :: prod A B`
* But with `{A B : Type}` the result of `mk 2 4 :: prod` meaning this is not a dependent type

```hs
inductive prodB {A B: Type} : Type 
| mk : A → B → prodB 
#check mk 2 4
-- mk 2 4 :: prod
```

coq
```hs
Inductive prod {A B: Type} : Type :=
  | mk (x:A) (y:B) .
Check mk 2 4. 
(* mk 2 4 :: prod     *)
```


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

* fst and snd projection functions are canonical functions of the product type

<!--  -->
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

* term Elimination for Sum type is Pattern matching in Haskell, Coq,

$$\cfrac{\vdash p: A + B \qquad  x: A \vdash v_A : C \qquad y : B \vdash v_B : C }{match(p,x.v_A,y.v_B):C} \tag{term elim}$$
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

# Pi vs Lambda vs forall

```hs
def injective {A B} (f: A -> B) :=
  Π x y : A, f x = f y -> x = y

def injective {A B} (f: A -> B) :=
  ∀ x y : A, f x = f y -> x = y

def injective {A B} (f: A -> B) :=
  λ x y : A, f x = f y -> x = y
```

# Notes

Poly

Polymorphic inductive type definition aka Inductive + Dependent types

implicit args

Because X is declared as implicit for the entire inductive definition including list' itself, we now have to write just list' whether we are talking about lists of numbers or booleans or anything else, rather than list' nat or list' bool or whatever; this is a step too far.

Tactics

All constructors/ term intro are injective aka rules that take arguments like successor takes argument n to make S n, and if S m = S n then n = m. Constructors that take no arguments are trivilaly injective like nil in list or 0 in nat.