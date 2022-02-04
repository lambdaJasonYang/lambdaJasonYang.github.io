---
title: Sequents and Gentzen Cut
tags: mathcs, logic
toc: y
---

# Terms

* Syntatic consequence: $p \vdash q$: sentence q is provable from the set of assumptions p.
* Semantic Consequence: $p \vDash q$: sentence q is true in all models of p.

In propositional logic, 

* Syntatic consequence : $p \vdash q$ = Propositional Calculus
* Semantic consequence : $p \vDash q$ = Truth tables


## Soundness

$$ p \vdash q \Rightarrow p \vDash q $$ 

Prove propositional calculus correct using 

## Completeness


$$a \vDash b$$  
$$\{x | a(x)\} \subset \{x | b(x)\}$$ 
a satisfies b  

$x :: U$  
$a, b :: U \rightarrow Bool$  
a,b are subsets of the universe U.  
Subset = Predicate = Function Universe U to Bool



$\Gamma :: \{Prop\}$  
$\Delta :: \{Prop\}$  
$\Gamma \Delta$ are the set of propositions in this universe.

```haskell
type Predicate u = u -> Bool

data Universe = A | B | C | D deriving (Eq,Show)

myuniverse = [A ,B ,C , D]

data Age = Old | Mid | Young deriving Eq

age :: Universe -> Age
age A = Old
age B = Old
age C = Mid
age D = Young

data Height = Tall | Short deriving Eq

height :: Universe -> Height
height A = Tall
height B = Short
height C = Tall
height D = Short

data Emotion = Happy | Sad deriving Eq

emotion :: Universe -> Emotion
emotion A = Happy
emotion B = Happy
emotion C = Sad
emotion D = Sad

happySet :: Predicate Universe
happySet x = emotion x == Happy

type Set = Predicate Universe

(|=) :: Set -> Set -> Bool
(|=) a b = and [b x | x <- myuniverse, a x]  

(|/=) :: Set -> Set -> Bool
(|/=) a b = not (a |= b)
```


---

# Gentzen Cut

Gentzen Cut is simply a syntatic trick

$$
\frac{a \vDash b \ \ \ \ b \vDash c}{a \vDash c}
$$

$$
\frac{ {\color{blue}\Gamma , a} \vDash b , {\color{red}\Delta} \ \ \ \ {\color{blue}\Gamma} , b \vDash{\color{red} c , \Delta}}{{\color{blue}\Gamma , a} \vDash  {\color{red}c , \Delta}}
$$

We expand 

$$
\frac{ {\color{blue}\Gamma , a} \vDash b , {\color{red}c , \Delta} \ \ \ \ {\color{blue}\Gamma, a} , b \vDash{\color{red} c , \Delta}}{{\color{blue}\Gamma , a} \vDash  {\color{red}c , \Delta}}
$$
Absorbing a into Gamma  
Absorbing c into Delta  
$$
\frac{\Gamma' \vDash b , \Delta' \ \ \ \ \Gamma' , b \vDash \Delta'}{\Gamma' \vDash  \Delta'}
$$

Why?
Remember 
$AND \vdash OR$
