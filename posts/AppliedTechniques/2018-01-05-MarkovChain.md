---
title: Markov Chain and State machines
tags: mathcs, AI, statistics
toc: y
---

# Markov Chains

Given a state machine labeled by probability of transitions.
We can find the most probability of landing on a certain state.

Example in a game, you are given 3 level up potions that probabilisticly grants from 1 to 10 levels depending on your current level. 
At a lower level there is higher chance of gaining more levels and etc.

The states are your current character level, and the probability transitions are described by the potion's effect.

We design a transition matrix that represents our state machine and multiple itself  3 times to get the 3-step transition matrix.
This will tells us the probability of landing on a certain level after taking 3 potions.

The steady state means at a certain step n, our transition matrix converges or simply put the probabilities don't change much. 
The T^n = \overset{k\rightarrow\inf}lim T^k 

Note not all state machines have a steady state

# Markov Simulation

Given a known probability distribution, simulate random selection.  
Then operate on the data.  

Example: