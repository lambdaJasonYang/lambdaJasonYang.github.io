---
title: Cointegration and Spreads
tags: tech,mathcs,AI,musings
---

Remember in Juypter notebook, use questionmark to get quick info
np.random.normal?

Pairtrading lecture:
Cointegration without Correlation
Example: Low frequency Sin wave compared to White noise, they both mean revert to 0 but are not correlated. In one increasing peak of the sine wave, there are hundreds of up and downs in white noise.

Playing the spread
The goal is make money betting that the spread either converges or diverges.
Spread = A-B

Possible biases
Bounding variable bias -> basically a confounding variable that is cointegrated with both timeseries you are looking at BUT the timeseries are not cointegrated with each other.

It is bad when your results show either:
Your pick is cointegrated with most other stocks
Your pick is cointegrated with the SP500.


Multiple Comparison Bias
Basically just means look out for the natural 0.05 alpha errors. with an alpha of 0.05, statistically 5% of our tests will show cointegration when there isnt.

Another method of Cointegration:
Perhaps there is different scaling between timeseries.
First we "normalize" one timeseries A to another B with OLS.
beta = OLS(A,B)
Spread = B - (beta * A)

Lookahead bias
Example 
You use data from [2000,2010] for beta = OLS(A,B) to get Spread.
Spread is sort of like an inference output variable.

You use that beta to calculate spread in [2008,2009]
The problem is beta is related to the solution and the timeframe [2000,2010] intersects and contains the future of the 
spread inference timeframe [2008,2009]
That is lookahead bias. 


You should only use that beta to calculate spread outside [2000,2010] timeframe.
Z-scores just tell you how many std dev from the mean you are.

you can z-score to graph a more intuitive chart of how price moves

WARNING, look ahead bias 
Even simple statistical tools like gettig the mean and std dev or z score means you are folding the entire data into solution.

A resolution to look ahead bias is 
Moving averages since they lag by default
instead of Beta = OLS(A,B) for entire window 
we get Rolling_beta = OLS(A,B) over a time period