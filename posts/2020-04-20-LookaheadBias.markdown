---
title: Lookahead Bias
tags: tech,mathcs,AI,musings
---

A frequentist statistican can easily make a mistake with lookahead bias.
The mindset of simply aggregating and summarizing data that is prevalent in many frequentists can lead to this mistake.
Example:

* Graphing a timeseries stock chart from 2008-2010, then drawing straight horizontal boundaries(the mean and the standard deviations) on the graph using the stock data.
* Then the frequentist says "whenever the price crosses these boundaries, we should act"
    * problem: the mean and std dev. is calculated using the total data. The correct chart can not be used to make any inferences from 2008-2010. 
    * Perhaps we can use this to predict 2011 if we used this entire chart as a 2 year moving average.     


Solution:
Use Moving averages or Timeseries statistics  