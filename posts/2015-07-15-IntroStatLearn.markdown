---
title: Intro To Statistical Learning Notes
tags: mathcs, appliedmath, AI, stats
---

`Wage`{.R}  : Linear Regression
How does `age`{.R} affect `wage`{.R}


`Smarket`{.R} : Classification
Classification 

### Chapter 2

below is the platonic ideal function that maps inputs $X$ to reality

$$ Y = f(X_1,X_2,...) + \epsilon $$

$$\epsilon := noise $$

We can't ever know the ideal $Y$ or $f$ so we estimate.  


$\hat{Y}  \hat{f}$ are our estimated variabes  
$e$ averages out to 0

$$ \hat{Y} = \hat{f}(X) $$




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

##### Conceptual

###### flexible vs understandable

---