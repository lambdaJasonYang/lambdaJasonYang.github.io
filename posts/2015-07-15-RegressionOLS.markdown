---
title: Regression and OLS
tags: mathcs, appliedmath, AI, stats
---
# Abstract

Goal: Find $\beta$ weight vector that minimizes sum square residual  

Method: 
1. Find inflection(min/max): Solve derivative of Error wrt. weight equal to 0 
2. Prove that this is a min, not a max: Check 2nd derivative is positive definite(analogous to postive in real numbers)  

# Reality vs Estimate

Ideal Model of Reality
$$\dot{Y} = X\dot{\beta} + \epsilon$$

Estimated Model
$$Y=X\beta + e$$

$$e \neq \epsilon$$

Conceptually there is no relation between $\epsilon$ and $e$

* $\epsilon$ is random noise.  
* $e$ is our residual error: vertical distance of datapoint from best-fit line   

> We can **ignore our Ideal Model**  $\dot{Y},\dot{\beta},\epsilon$ because they are simply model of an impossible ideal.  

# Finding best fit

Regression is the best fit line that is minimizes $e$ or residual squared error.

$$Y=X\beta + e$$

$$e=Y-X\beta$$



## RSS: Residual Sum Squared

Sum of Squared residuals

$$e^Te = (y-X\beta)^T(y-X\beta)$$

$$\frac{\partial e^Te}{\partial\beta}=-2X^Ty+2X^TX\beta = 0$$

$$(X^TX)\beta=X^TY$$