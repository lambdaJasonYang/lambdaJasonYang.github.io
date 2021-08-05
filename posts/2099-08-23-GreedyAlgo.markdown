---
title: Greedy Algo
tags: mathcs, cs, math, algorithms
---
### Greedy Swap Proof
#### Example problem: 
$$ A = [a_1 , a_2, a_3 ...]$$
$$ O = [o_1, o_2, o_3 ...]$$
Each element of A represents a state.  
$$ Assume\ A \neq O $$ (NOTE THIS IS NOT A PROOF BY CONTRADICTION AS THERE MAY BE MULTIPLE OPTIMAL SOLUTIONS THAT ARE NOT EQUAL)
Therefore
$$ Either\ $$
$$\exists x \in O , x \notin A $$
$$ \exists x \in A, x \notin O$$
$$ a_p = o_q , a_q = o_p \ Swap$$



### Greedy Inductive Lookahead Proof
#### Example problem: A frog can take 1..k Steps each turn, get least number of turns to reach the n Step.

State = turn  
Greedy Solution, take the max k Step each State  

Proof of Correctness:
Show by induction that for all states, the current state is the most optimal  


```bash
chcp 65001
stack exec myblog clean
stack exec myblog build
```

```bash
git add .
git commit -m "some message"
git push origin main:main
```



``` haskell
fac n = foldr (*) 1 [1..n]
```

```{.ruby .numberLines}
def greet; "Hello, world!"; end
```
