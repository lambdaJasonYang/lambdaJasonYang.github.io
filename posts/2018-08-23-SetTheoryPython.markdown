---
title: Building Natural numbers from Sets with python
tags: math, CS
---


[Jupyter Note book python implementation using default set data structure](https://nbviewer.jupyter.org/github/userJY/JupyterNotebooks/blob/master/SetTheory.ipynb)

We can axiomatically build the Set-interpretation of natural numbers by recursively nesting sets within each other.

$$ \{  \} = \emptyset := 0 $$
$$ 0 \cup \{ 0 \}  = \{ 0 \} = \{ \{ \} \}  := 1  $$
$$ 1 \cup \{1 \} = \{ 0, 1 \} = \{ 0, \{ 0 \} \} = \{ \{ \} ,\{ \{ \} \} \} := 2 $$
$$ 2 \cup \{2 \} = \{ 0, 1, 2 \} = \{ 0, 1 \} \cup \{ \{ 0, 1 \} \} = \{ \{  \},\{ \{ \} \} , \{ \{ \} ,\{ \{ \} \} \}  \} = 3 $$

$$ k-1 \cup \{ k-1 \} = \{ 0,1,2... k-1 \} := k$$

Given a goal to define the Set-interpretation of the k natural   

We take the Set-interpretation of the (k-1) natural, which we have previously defined, and union it with itself but nested inside a set.

Another way to look at it.  
To define the k natural, we form a set that contains all the Set-interpretation of numbers from 0 to k-1.

Given the above knowledge we can define a successor function:
$$ Succ(K) = K \cup \{ K \} $$

Notice that since the k natural contains all the numbers,(0,1,2...k-1) before it,  
we can define a Predecessor function by simply fetching the Maximum element inside the Set-interpretation of K.
$$ Pred(K) = max(\{ x | x \in K \}) $$