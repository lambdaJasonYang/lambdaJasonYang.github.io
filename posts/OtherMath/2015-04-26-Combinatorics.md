---
title: Combinatorics
tags: mathcs, puremath, recursion
toc: y
---


Combinatorics is all about counting injective functions.

| Term | Term | Term | Calc | Term |
| --- | --- | --- | --- | --- |
| ~3~**P**~3~ | Bijection aka (Injection w/ Domain == Range) | 3->3 | 3! | Factorial |
| ~5~**P**~3~ | Injection w/ Domain < Range | 3->5 | 5!/2!  | Restrictive counting w/ no repeats |
| ~5~**C**~3~ | Combination aka Injection up to permutative isomorphism | 3->5 | 5!/2!3! | Restrictive counting w/ no repeats + normalize permutative isomorphisms |


* Combinations we use *'s to represent elements of domain because the identity of the domain doesn't really matter
* Permutations the identity of the domain does matter.

# Non-Injective

```text
                             +--------------+
                             |    States    |
   Objects                   |              |
                             |              |
       -----------------------------|       |
       |                     |      |       |
+------|----+                |      |       |
|      |    |                |      |       |
|      |    |                |      v       |
|  PropA    |           ---------->True     |
|           |         --|    |              |
|           |         |      |              |
|       --------------|      |              |
|  PropB    |                |              |
|           |                |  -> False    |
|           |                |  |           |
|           |                |  |           |
|  PropC -----------------------|           |
|           |                |              |
|           |                |              |
+-----------+                +--------------+
```


$$ \{A,B,C\} \mapsto \{T,F\} \qquad \{T,F\}^{\{A,B,C\}}=2^3 \text{ functions}$$
$$ | P(\{A,B,C\}) | = |\{\emptyset,\{A\},\{B\},\{C\},\{A,B\},\{A,C\},\{B,C\},\{A,B,C\}\}| = 2^3$$
$$\text{True represents included in set, False represents not included in set}$$

* Counting the possible of functions from Object to State is counting the number of possible configurations.
* Propositions can be either in the True state or False state.


```bash
Map {T,F} <- {A,B,C}
 T,F    Depth
  /\      A
 /\/\     B
/\/\/\    C
```
In our tree, we decide branching represent T,F and Depth represent the A,B,C.  
A path from root to leaf represents 1 function.  
  Example the far left path is (A True-Right branch, B True-Right branch, C True-Right branch)

# Combinations

```text
                         +--------------+              +--------------+    
+------------+           |              |              |              |    
|            |           | +---------+  |              | +---------+  |    
|+-----+     |           | |  Red    |  |     How many | |  Red    |  |    
|| *   |     | 3 elem    | |+---------+ |     3 elem   | |+---------+ |    
||     |  ------Inject-->| || Blue   || |     subsets  | || Blue   || |    
|| *   |     |           | ||        || |     can we   | ||        || |    
||     |     |           | ||        || |     make?    | ||        || |    
|| *   |     |           | || Green  || |              | || Green  || |    
|+-----+     |           | +|--------+| |              | +|--------+| |    
|            |           |  | Yellow  | |              |  | Yellow  | |    
+------------+           |  +---------+ |              |  +---------+ |    
                         +--------------+              +--------------+    
```

* Combination we reverse the Domain and Range.
* 3 picks 2 means we count the number of **injective** functions from 2 to 3 **up to mapping isomorphism**
  * Up to mapping isomorphism means f = (T-> A,F->B) is counted as the same as g = (F -> A, T->B).

Taking only Up and Right moves, how many ways can you go from bottom left to top right?

```bash
+--+--+
|  |  |
+--+--+
```

Naive attempt, observe we can't just go Up 3 times. 

```bash
Map {Up(U),Right(R)} <- {state 1,state 2, state 3}
  U,R    depth
  /\     state 1
 /\/?    state 2
/??      state 3
```

* Notice we only need 1 Up(1 U) and 2 Rights(2 R*). 
* Two movements cannot occupy the same state, therefore injective
* Movement -> State
  * We need to normalize for the 2 Rights(R* )

3 perspectives, 3 equally valid solutions

* Focus on 1 Up: Once we choose which state is Up, the other Rights get locked in.
  * 3 pick 1
* Focus on 2 Right*: Once we choose which 2 state is Right*, the Up get locked in.
  * 2 Right* and order doesn't matter meaning we need to normalize for Right* permutation = 2!
  * (3 pick 2)/(2!)
* Focus on all 3: We give a state for all 3.
  * We have to normalize for the 2 Right* permutation = 2!
  * (3 pick 3)/(2!)

# Examples

* Propositions can either be true or false



```txt
            Wrong         ┌───────────┐               Wrong      ┌──────────────┐
                          │           │                          │              │
     ┌────────────────────┼───────┐   │                          │              │
     │                    │       │   │                          │              │
┌────┼────┐               │       │   │       ┌──────────┐    ┌──┼─►  Fst       │
│    │    │               │       │   │       │          │    │  │              │
│    │    │               │       ▼   │       │     ┌────┼────┘  │              │
│ Fst│    │           ┌───┼─────► Up  │       │     │    │       │              │
│         │         ┌─┘   │           │       │     │    │       │              │
│         │         │     │           │       │   Up│    │       │    Snd       │
│     ────┼─────────┘     │           │       │          │       │              │
│ Snd     │               │           │       │          │       │              │
│         │               │   ┌► Right│       │          │       │              │
│         │               │   │       │       │          │       │              │
│         │               │   │       │       │   Right ─┼───────┼──► Thrd      │
│ Thrd ───┼───────────────┼───┘       │       │          │       │              │
│         │               │           │       │          │       │              │
│         │               │           │       │          │       │              │
└─────────┘               └───────────┘       └──────────┘       └──────────────┘
```


From a bag of 5 different balls, you pick 3, how many combos can you make?

```txt
                                    ┌───────────────────────────┐
                                    │                           │
                                    │                           │
                                    │                           │
┌──────────────────────┐            │                           │
│                      │            │         5*4*3 ways        │
│                      │            │          to choose        │
│    ┌─────────────────┼────────────┼───────────────────┐       │      Normalize
│    │                 │            │                   │       │    by Permutation
│    │   BallA         │            │           1       │       │
│    │                 │            │                   │       │   div 3!         5*4*3
│    │                 │            │                   │ ──────┼───────────────►  ------
│    │   BallB         │   chosen   │           2       │       │                  3*2*1
│    │                 │            │                   │       │
│    │                 │            │                   │       │
│    │   BallC         │            │           3       │       │
│    └─────────────────┼────────────┼───────────────────┘       │
│                      │            │                           │
│    ┌─────────────────┼────────────┼───────────────────┐       │
│    │                 │            │                   │       │
│    │   BallD         │            │           4       │       │                  5*4
│    │                 │ not chosen │                   ├───────┼───────────────► ------
│    │                 │            │           5       │       │    div 2!        2*1
│    │   BallE         │            │                   │       │
│    └─────────────────┼────────────┼───────────────────┘       │
│                      │            │             5*4 ways      │
│                      │            │           to NOT choose   │
└──────────────────────┘            │                           │
                                    │                           │
                                    └───────────────────────────┘
```



# PIE

## Counting

To count A∨B we alternate ADD,SUB for each layer below   
To count A∧B we alternate ADD,SUB for each layer above  

```txt           
                    Layer 
       A∨B          0    
    A       B        1    
       A∧B          2     
```

* A∨B [layer 0] 
  * ADD (A + B) [layer 1]
  * SUB A∧B [layer 2]

Likewise

* A∧B
  * ADD (A + B)
  * SUB A∨B

```txt
           A∨B∨C         
      A∨B   A∨C   B∨C
        A     B     C
      A∧B   A∧C   B∧C
           A∧B∧C
```

To derive A∨B∨C, we sum everything below while alternating signs for each layer

* A∨B∨C
  * ADD (A∨B + A∨C  + B∨C)
  * SUB (A + B + C)
  * ADD (A∧B + A∧C + B∧C)
  * SUB (A∧B∧C) 
 
A∨B∨C = +(A∨B + A∨C  + B∨C) -(A + B + C) +(A∧B + A∧C + B∧C) -(A∧B∧C)  

We can do the same for A∧B∧C

* A∧B∧C
  * ADD (A∧B + A∧C + B∧C)
  * SUB (A + B + C)
  * ADD (A∨B + A∨C  + B∨C)
  * SUB (A∨B∨C)

## Inverses

Criss-cross, A∨B on top can be derived from it's opposite ¬A∧¬B on bottom.

Universe U is Union of everything(letter U also looks like Union ∪)    
U = A∪B∪C..∪¬A∪¬B∪¬C..

```txt           
Assume universe U contains everything

      A∨B      \   /      ¬A∨¬B
    A      B     \/     ¬A       ¬B
      A∧B       / \       ¬A∧¬B

    Normal                Inverse
```
* U - A∨B = ¬A∧¬B
* U - A = ¬B
* U - B = ¬A
* U - A∧B = ¬A∨¬B


### Example

Let's say the universe U is all naturals divisible by 1000 and we want to know the count of naturals not divisible by 2 or 3.

* U = Nat Less than 1000
* A = divisible by 2
* B = divisible by 3
* A∨B = divisible by 2 or 3

2 methods to solve this, either  
Method1: use PIE on normal symbols then subtract by universe U to invert it.  
Method2: reduce with Demorgan then solve it using PIE on the inverted symbols.

* Method 1: No reduction
  * Goal: ¬(A∨B) = U - A∨B      
  1. A∨B = +(A + B) -(A∧B)     
      * Use PIE on normal symbols {A,B,A∧B}
  2. solve by subtracting universe U to invert (A∨B)
* Method 2: Reduce with DeMorgan's law
  * Goal: ¬(A∨B) = ¬A∧¬B
  1. ¬A∧¬B = +(¬A + ¬B) -(¬A∨¬B)   
      * Use PIE on inverted Symbols {¬A,¬B,¬A∨¬B}
  2. solve.. Done (¬A∧¬B)