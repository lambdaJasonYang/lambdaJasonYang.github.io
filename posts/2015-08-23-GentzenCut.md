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

| Sequent | Logic | 
| --- | --- |
| $a\vDash b$ | $\forall x \in a, x \in b$ | 
| $a \vDash \lnot b$ | $\forall x \in a, x \notin b$| 
| $\lnot a \vDash b$ | $\forall x \notin a, x \in b$ |
| $a \nvDash b$  | $\lnot (\forall x \in a, x \in b)$ |


$x :: U$  
$a, b :: U \rightarrow Bool$  
a,b are subsets of the universe U.  
Subset = Predicate = Function Universe U to Bool



$\Gamma :: \{U \rightarrow Bool\}$  
$\Delta :: \{U \rightarrow Bool  \}$  
$\Gamma, \Delta$ are the set of propositions in this universe.

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
\frac{a \vDash b \ \ \ \ b \vDash c}{a \vDash c}\tag{transitive implication}
$$

$$
\frac{ \Gamma , a \vDash b , \Delta \ \ \ \ \Gamma , b \vDash c , \Delta}{\Gamma , a \vDash  c , \Delta}
\tag{append gamma and delta to both sides}$$


$$
\frac{ {\color{blue}\Gamma , a} \vDash b , {\color{red}c , \Delta} \ \ \ \ {\color{blue}\Gamma, a} , b \vDash{\color{red} c , \Delta}}{{\color{blue}\Gamma , a} \vDash  {\color{red}c , \Delta}}
\tag{Append a with Gamma, c with delta} $$

$$
\frac{\Gamma' \vDash b , \Delta' \ \ \ \ \Gamma' , b \vDash \Delta'}{\Gamma' \vDash  \Delta'}\tag{absorb a with Gamma, c with delta}
$$


## Explanation

*  The Append step is the most confusing because we ask why is this legal in ${\color{blue}\Gamma , a} \vDash b , {\color{red}c , \Delta}$?  
    * Why can we arbitrarily convert $..\vDash b , \Delta$  to $..\vDash b , {\color{red}c , \Delta}$
    * (Cloudy days) implies (rain **OR anything else**)

$$ \frac{x_1 \vDash y_1}{x_1 \vDash y_1,y_2,y_3,...}$$

* The same could be asked about ${\color{blue}\Gamma, a} , b \vDash{\color{red} c , \Delta}$
    * Why can we arbitrarily convert $\Gamma,b \vDash ..$ to ${\color{blue}\Gamma, a} , b \vDash ..$
    * (Cloudy days **AND anything else**) implies (rain)

$$ \frac{x_1 \vDash y_1}{x_1,x_2,x_3,.. \vDash y_1}$$

* Summary: Remember that a sequent $x_1,x_2 \vDash y_1,y_2$ is logically equivalent to $x_1 \land x_2 \rightarrow y_1 \lor y_2$
  * We can keep ANDing more "vacuous evidence" on the left side of the implication 
  * We can keep ORing more "vacuous proofs" on the right side of the implication


Remember $AND \vDash OR$
