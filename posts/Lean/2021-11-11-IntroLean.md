---
title: Intro Lean
tags: lean, mathcs
toc: y
---

# Notation


## Type Parameters vs Arguments

`def bleh (PARAMETER1 : Type) (PARAMETER2 : Type) : ARGUMENT1 -> ARGUMENT2 := ...`

* PARAMETER: Before the main semicolon 
* ARGUMENT: After the main semicolon 
<!--  -->
* Example `def Bigger (x : Nat) (y : Nat): Bool := ...`
  * Parameter: `(x : Nat) (y : Nat)`
  * Argument: `Bool`
<!--  -->
* PARAMETER and ARGUMENTS do not behave in the same way in Function vs Inductive Type definitons


# Forall, Pi

* Π and ∀ are interchangable. 

```hs
constant consA :  Πa  : Type u , a -> list a -> list a
constant consB : ∀a  : Type u , a -> list a -> list a
constant consC : ∀(a : Type u), a -> list a -> list a 
constant consD : Π(a : Type u) , a -> list a -> list a
constant consE (a : Type u) : a -> list a -> list a
```

```hs
def injective {A B} (f: A -> B) :=
  Π x y : A, f x = f y -> x = y
def injective {A B} (f: A -> B) :=
  ∀ x y : A, f x = f y -> x = y
def injective {A B} (f: A -> B) :=
  λ x y : A, f x = f y -> x = y
```


# Inductive type


* An inductive type aka inductive datatype aka algebraic datatype is a type that consists all the
values that can be built using a finite number of applications of its
constructors and only those.
<!--  -->
* `Inductive` keyword bundles together Type formation and Term introduction
  
```hs
inductive nat : Type --type formation
| zero : nat --term introduction
| succ : nat → nat --term introduction
```

```hs
constant nat : Type --type formation
constant nat.zero : nat --term introduction
constant nat.succ : nat → nat --term introduction
```

## Inductive Type parameters

* By adding parameters, we can have Inductive dependent types meaning the constructors are dependent typed functions.
* **Notice the last Type of the constructor `mk :..-> prod` is the inductive type itself `prod`**
  * Constructors must always refer back to the inductive type

```{.c group="g1" glabel="lean"}
inductive prod (A B : Type) : Type
  | mk : A -> B -> prod
#check prod.mk 2 4 
OUTPUT> mk 2 4 :: prod nat nat
```

```{.hs group="g1" glabel="coq"}
Inductive prod (A B:Type) : Type :=
  | pair (x:A) (y:B) : A -> B -> prod A B.
Check mk nat nat 2 4. --notice how this diff from Lean's mk
```

## dependently typed constructors

* Instead of having typed parameters like `prod (A B : Type) : Type` we use dependently typed constructors like `mk : Π ( A B : Type )...`

```{.c group="g2" glabel="lean"}
inductive prod : Type -> Type -> Type 1
  | mk : Π (A B : Type), A -> B -> prod A B
#check mk nat nat 2 4 
OUTPUT> mk nat nat 2 4 :: prod ℕ ℕ
#check mk
OUTPUT> mk :: Π (A B : Type), A -> B -> prod A B
```

```{.c group="g2" glabel="coq"}
Inductive prod :  Type -> Type -> Type :=
  | mk : forall A B :Type , A -> B -> prod A B .
Check pair nat nat 2 4.
OUTPUT> pair nat nat 2 4 :: prod nat nat 
Check mk.
OUTPUT> mk :: forall A B : Type, A -> B -> prod A B
```

## Parameter Behavior in functions

* Observe how `(m : ℕ)` as a parameter behaves
  * argument m of `powerB m` is fixed inside the definition   
  but outside the definition, like in `eval powerB 2 5`, m is not fixed

```hs
def power : ℕ → ℕ → ℕ
| _ nat.zero     := 1
| m (nat.succ n) := m * power m n
#eval power 2 5

def powerB (m : ℕ) : ℕ → ℕ
| nat.zero     := 1
| (nat.succ n) := m * powerB n --notice we can omit argument m here
#eval powerB 2 5 
```

## Structures

**Structures are non-recursive inductive data types**  
In other langs, they are called records   

# Implicit vs Explicit parameters

```hs
constant consImplicit {a : Type u } :  a -> list a -> list a 
constant nilImplicit : Π {a : Type u}, list a
#check consImplicit 
OUTPUT> ?M_1 → list ?M_1 → list ?M_1 

--using @ fills in the metavar with types
#check @consImplicit
```


# Recursion

* If a function uses a recursive call to itself lean must also provide a proof of well-foundedness
  * Contrary to Coq which does not require a proof

# Intro

## Parameters vs Arguments

