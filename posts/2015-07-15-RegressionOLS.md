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
$$Y = X\beta + \epsilon$$

Estimated Model
$$Y=X\hat{\beta} + e$$

* Population analogous to Reality
* Sample analogous to a simulation or model of reality

Statistics is about using Samples(Simulation/Models/Estimations) to estimate Population(Reality)

## Residual vs Error


$$X_1 .. X_n \sim N(\mu ,\sigma^2)$$
$X_1..X_n$ each represent the choice of a random singleton subset AKA singleton sample from population.  
$\mu$ is the population mean.  

$$\bar{X} = \frac{X_1 + .. X_n}{n}$$
$\bar{X}$ is sample mean. Notice it is a random variable because our choice of sample subset is random which also implies a random meam for each of these possible subsets.

$$\bar{X} \sim N(\mu, \sigma^2)$$
$$\epsilon_i = X_i - \mu$$
$$ e_i = X_i - \bar{X}$$

$$e \neq \epsilon$$

Residuals $e$ are basically estimates of Error $\epsilon$


* $\epsilon$ is random noise of reality we typically can't measure. 
  * Error is random error from population data.   
* $e$ is our residual error: vertical distance of datapoint from best-fit line   
  * Residual is deviation between our estimated model and sample data.

> We can **ignore our Ideal Model**  $\dot{Y},\dot{\beta},\epsilon$ because they are simply model of an impossible ideal.  

# Finding best fit

Regression is the best fit line that is minimizes $e$ or residual squared error.

$$Y=X\hat{\beta} + e$$

$$e=Y-X\hat{\beta}$$



## RSS: Residual Sum Squared

Sum of Squared residuals

$$RSS(\beta) = e^t e $$

$$e^Te = (Y-X\hat{\beta})^T(Y-X\hat{\beta})$$

Set derivative to 0 to find inflection point

$$\frac{\partial e^Te}{\partial\beta}=-2X^TY+2X^TX\hat{\beta} = 0$$

$$(X^TX)\hat{\beta}=X^T Y$$

$$\hat{\beta}=(X^T X)^{-1} X^T Y$$

$$\hat{\beta} = (X^T X)^{-1} X^T ( X \beta + \epsilon )$$

Show that this inflection point is minimum by proving the 2nd derivative is positive (or positive definite for matrices).

$$\frac{\partial^2 e^Te}{\partial\beta \partial\beta^T} = 2X^T X$$  
Assuming X has full column rank, $X^T X$ can be shown to be positive definite.


## Covariance matrix

The $X^T X$ is the Covariance Matrix in multivariate normal distribution.

$$\Sigma = Cov(X,X) = (X_i - \mu_i)(X_j-\mu_j)



