---
title: Combinatorics
tags: mathcs, puremath, recursion
toc: y
---



$$ \{A,B,C\} \mapsto \{T,F\} \qquad \{T,F\}^{\{A,B,C\}}=2^3 \text{ functions}$$
$$ | P(\{A,B,C\}) | = |\{\emptyset,\{A\},\{B\},\{C\},\{A,B\},\{A,C\},\{B,C\},\{A,B,C\}\}| = 2^3$$

* If we give each of 5 objects 3 different states, how much configurations are possible? 
  * This is just counting the number of functions from 5 objects to 3 states.

```bash
 T,F    Depth
  /\      A
 /\/\     B
/\/\/\    C
```
In our tree, we decide branching represent T,F and Depth represent the A,B,C.  
A path from root to leaf represents 1 function.  
  Example the far left path is (A True-Right branch, B True-Right branch, C True-Right branch)

# Combinations

Taking only Up and Right moves, how many ways can you go from bottom left to top right?

```bash
+--+--+
|  |  |
+--+--+
```

Naive attempt, observe we can't just go Up 3 times. 

```bash
  U,D    depth
  /\     1 steps
 /\/?    2 steps
/??      3 steps
```