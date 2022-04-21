---
title: Decision Minimax/min
tags: musings, misc
toc: y
---

| Present Choice |  Best future | Typical future | Worst future |
| --- | --- | --- | --- |
| invest 8 | +8 | +2 | -4 |
| invest 4 | +4 | +1 | -2 |
| invest 2 | +2 | +0.5 | -1 |
| invest 1 | +1 | +0.25 | -0.5 |
| No action | 0 | 0 | 0 |

# Maximin

$$Decision = Min(Worst_Future)$$
$$Decision = Min({-4,-2,-3,-0.5,0\}) = 0$$ 
$$Decision = No\ Action$$

# Maximax

Choose the decision Max of Best future col which is +8 corresponding to "invest 8"

$$Decision = Min(Best_Future)$$
$$Decision = Min({+8,+4,+2,+1,0\}) = 0$$ 
$$Decision = invest\ 8$$

# Regret

| Present Choice |  Best future | Typical future | Worst future |
| --- | --- | --- | --- |
| invest 8 | +8 -8 | +2 -2| -4 -0.5|
| invest 4 | +4 -8| +1 -2| -2 -0.5|
| invest 2 | +2 -8| +0.5 -2| -1 -0.5|
| invest 1 | +1 -8| +0.25 -2| -0.5 -0.5|
| No action | 0 -8| 0 -2| 0 -0.5|

* Regret for a single column = Current returns - Max returns  
* This means the best decision in a chosen future should have 0 regret.

# Expected value 

Each future has a probability. So we calculate expected value for each investment choice.
