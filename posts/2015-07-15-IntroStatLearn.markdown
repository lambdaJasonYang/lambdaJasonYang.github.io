---
title: Intro To Statistical Learning Notes
tags: mathcs, appliedmath, AI, stats, notes
toc: y
---

`Wage`{.R}  : Linear Regression
How does `age`{.R} affect `wage`{.R}


`Smarket`{.R} : Classification
Classification 

# terms

## Variance

variance is squared(distance-to-mean)  
That means there is larger value for scattered points than clumped points.  
The square operation has an absolute value like effect.  

TSS = Total sum squared = sum of variance of y's

Normalising against variance by dividing by TSS.  

# Chapter 2 OLS 

Goal: Find $\beta$ weight vector that minimizes sum square residual  

Method: 
1. Find inflection(min/max): Solve derivative of Error wrt. weight equal to 0 
2. Prove that this is a min, not a max: Check 2nd derivative is positive definite(analogous to postive in real numbers)  

## Reality vs Estimate

Ideal Model of Reality
$$Y = X\beta + \epsilon$$

Estimated Model
$$Y=X\hat{\beta} + e$$

* Population analogous to Reality
* Sample analogous to a simulation or model of reality

Statistics is about using Samples(Simulation/Models/Estimations) to estimate Population(Reality)

## Residual vs Irreducible Error


$$X_1 .. X_n \sim N(\mu ,\sigma^2)$$
$X_1..X_n$ each represent the choice of a random singleton subset AKA singleton sample from population.  
$\mu$ is the population mean.  

$$\bar{X} = \frac{X_1 + .. X_n}{n}$$
$\bar{X}$ is sample mean. Notice it is a random variable because our choice of sample subset is random which also implies a random meam for each of these possible subsets.

$$\bar{X} \sim N(\mu, \sigma^2)$$

$$E(Y_i) = \mu$$
$$\epsilon_i = Y_i - E(Y_i) \text{ typically impossible to find}$$


$$ e_i = X_i - \bar{X}$$



$$e \neq \epsilon$$

Residuals $e$ are basically estimates of Error $\epsilon$


* $\epsilon$ is random noise of reality we typically can't measure. 
  * Error is random error from population data.   
* $e$ is our residual error: vertical distance of datapoint from best-fit line   
  * Residual is deviation between our estimated model and sample data.

> We can **ignore our Ideal Model**  $\dot{Y},\dot{\beta},\epsilon$ because they are simply model of an impossible ideal.  

## Finding best fit

Regression is the best fit line that is minimizes $e$ or residual squared error.

$$Y=X\hat{\beta} + e$$

$$e=Y-X\hat{\beta}$$



### RSS: Residual Sum Squared

Sum of Squared residuals

$$RSS(\hat{\beta}) = e^t e $$

$$e^Te = (Y-X\hat{\beta})^T(Y-X\hat{\beta})$$

Set derivative to 0 to find inflection point

$$\frac{\partial e^Te}{\partial\beta}=-2X^TY+2X^TX\hat{\beta} = 0$$

$$(X^TX)\hat{\beta}=X^T Y$$

$$ \hat{Y} = X\hat{\beta}=(X^T X)^{-1} X^T Y$$

$$\hat{Y} = (X^T X)^{-1} X^T ( X \beta + \epsilon )$$

Show that this inflection point is minimum by proving the 2nd derivative is positive (or positive definite for matrices).

$$\frac{\partial^2 e^Te}{\partial\beta \partial\beta^T} = 2X^T X$$  
Assuming X has full column rank, $X^T X$ can be shown to be positive definite.


### Covariance matrix

The $X^T X$ is the Covariance Matrix in multivariate normal distribution.

$$\Sigma = Cov(X,X) = (X_i - \mu_i)(X_j-\mu_j)




## Residual

$e$ is the residual.  
$e$ is not related to $\epsilon$ is anyway.


$$ E[Y-\hat{Y}]^2 = E[f(X)+\epsilon -\hat{f}(X)]^2 = {\color{red}E[f(X)-\hat{f}(X)]^2} + Var(\epsilon)$$
Overarching Goal: find $f$ st. term in RED is minimized


Steps: 

1. Choose model: Linear equation

$$ Y = f(X_1,X_2,...) + \epsilon$$ 

$f$ is matrix multiplication with weight vector $\beta$

$$Y = X\beta + \epsilon$$

2. Train parameters: Ordinary Least Squares(OLS)


```R
a <- c(2,3,5,6)

library(ISLR)



x <- rnorm(50,mean = 0,sd = 1)  # creates 50 points
y <- x + rnorm(50,mean=1,sd=0.5) # creates 50 points 
cor(x,y) # correlation
#> 0.8344
```
## Validate linearity

* We can use linear regression on any dataset but that DOES NOT imply a linear relation exists.  
* We must perform t-test(single independent var) or ANOVA F-test(multiple independent var) on the linear regression model's coefficients to prove whether a linear relation exists.

### T-test

* T-dist is a probability dist like a normal-dist but bigger tails
  * infinite degree of freedom result in T-dist = normal-dist
* smaller degree of freedoms imply bigger tails
* Can be useful to model returns with fat fails

Null H: There is no linear relationship between independent variable and output $\beta = 0$  
Alt H: There is a linear relationship between independent variable and output $\beta \neq 0$

**We do a T-test for EACH coefficient.**  
What happens when only some coefficient are significant and others arent.  
ANSWER: Typically, we throw out nonsignificant (high p-value) coefficients to sacrifice some bias to reduce variance.  
Remember high variance leads to overfit.  



#### degree of freedom

### Validate multi-linear regression model

#### F-test

Null H: None of the independent variable have a linear relationship with output $\forall n, \beta_n = 0$  
Alt H: At least one of the independent variable have a linear relationship with output$\exists n, \beta_n \neq 0$

Variance WITHIN groups vs Variance BETWEEN groups

Example: 

High WITHIN group Variance

* Height: 10, 9, 12
* Weight: 92, 95, 93
* Age: 20, 22, 21

High BETWEEN group Variance

* Height: 15, 30

## Model fit

### MSE

* MSE(Mean Squared Error) : used to check how well our regression model fits  
  * Add the squared distance from point to regression, then divide by count of datapoints

Goal is to minimize MSE of test dataset, not training dataset

#### Standard Error SE

* how far the sample {mean,weights,residuals} are from the reality or population {mean,weights,residuals}

#### RSE = SE(RSS)

Standard Error of RSS is RSE


### R^2

$R^2$ is a number in [0,1] and related to Variance(Scatter or Clumped)  
$R^2$ measure how scattered or clumped the data WRT our regression line aka model.

* Close to 1 means every datapoint is on our regression line
* Close to 0 means the datapoint are scattered but broadly follow trend with our regression line.

Optional: Drop coefficients aka Independent Random variables that do not contribute well in $R^2$ **despite having low p-value**

Example: 
Age variable gives $R^2$ 0.7 with p-value: 0.04
Adding a height variable gives $R^2$ 0.71 with p-value 0.005  
We can safely drop height variable

Cons:  

 * `R^2` increases as you add more parameters
   * Solution: used `Adj-R^2`

### Cor

$$R^2 = Cor(Y,\hat{Y})^2 \text{ for multiple lin reg}$$  
Linear fit models aim to $max(Cor(Y,\hat{Y}))$


## Prediction

Prediction interval

# Chapter 4 Classification

Why can't we just do one-hot encoding with linear regression?  
We Can but only for binary outcomes 

If we do it for more than binary outcomes, we create an equidistant ordering which may not reflect reality.

# Chapter 5 Resampling

Remember statistics is about how using our subset/sample to understand the population.  

Resampling is about repeatedly taking different subsets or samples. 

Two common resampling method: Cross-validation, bootstrap