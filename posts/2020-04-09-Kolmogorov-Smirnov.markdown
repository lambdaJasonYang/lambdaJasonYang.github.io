---
title: Testing for normal dist with Kolmogorov-Smirnov
tags: mathcs, appliedmath, musings
---





Kolmogorov-Smirnov test- are your returns normally distributed?
Why?
Value at risk, Modern portfolio theory, CAPM assume returns of assets are normally distributed

Get the percent daily returns
X [0.2,0.1,0.3 ...]

Sort the returns 
B = Sorted(X) = [0.1,0.2,0.3 ...]

enumerate the returns 
C = Enum(B) = [1,2,3, ...]

Divide C by len(X)
D = [1/300,2/300,3/300 ...] 
D  is called empirical distribution

Get Mean and Standard deviation of returns (B or C)
Using mean , get normal distribution of B
E = [normDist(0.1, 0.15, 0.01), normDist(0.2, 0.15, 0.01) ...]
E is called theoretical distrbution

We plot 2 series, B vs D, B vs E

Get difference of D and E
F = D - E 

Get maximum of differences aka max(F)
supremum = max(F)

How bad does normal distribution fit the data
Kolmogorov-Smirnov statistic = supremum * sqrt(len(X))

Critical value is 1%, if our Kolmogorov-Smirnov statistic > 1, then our data is not normally distributed