---
title: Options Derivatives Notes
tags: mathcs, finance
---

Assumption rate of stock price growth is exponential

# 2

First part Pricing

Basic securities stock bonds  
pricing known pay offs - what is the value today? answer is deterministic
 * pricing fixed cash flows
 * spot interest rates
 * forward rates

Similar principle can be used in stochastic payoffs?
 * options
 
 assumptions
 * Pricing by No-arbitrage
 * Binomial trees model - there is a unique price for each random/nonrandom payoffs. (Reality there is a bid ask spread )
 * Stochastic calulus, Ito rule, Brownian motion - Math tools to learn black scholes
 * Black-Scholes formula
 
Second part Hedging 


Fixed income derivatives - bond market

# 3

Futures and forwards are linear function of basic securities/stocks  

Swaps are linear function of basic securities. Swaps are just sequence of future contracts.  

Options are nonlinear functions of stocks.  


# 4

### Forward Contract 

F := forward price
T := maturity date
S(T) := spot (market) price at maturity

Forward price(F) is chosen so contract has 0 value today.
Payoff at maturity
 * long := S(T) - F = spot - forward
 * short := F - S(T) = forward - spot

* meta-simplify fancy terms
 * spot = current value  
 * forward = store bought price  

### Options 
S(T) = current value
K = strike 

current value < strike implies 0 payoff

 
before maturity options are smooth nonlinear functions and they grow closer to looking like piecewise functions.

calls flipped vertically on it's kink results in a put

long flipped  horzontally on x-axis results in a short.

All Longs have negative premiums. (horizontal line below 0)
All Shorts have positive premiums. (horizontal line above 0)



#5

swaps

#6 

Exotic options
* Asian options - value is average over period
* Lookback option - value is max or min over period
* Barrier option - value is binary, only pays if goes above a barrier (cheaper to buy but riskier)
* Basket option - value depends on value of several assets

Writing an option - selling option
Premium - price of option

#7

#10
Theoretically we can design any curve assuming we call options are traded for ALL(infinitely many) strike prices(unrealistic).

#21


# 45 

Fundamental Theorem of asset pricing

