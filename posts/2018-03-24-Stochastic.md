---
title: Stochastic calculus and brownian motion
tags: prog, python, mathcs, appliedmath
---

---
title: Stochastic calculus
tags: tech
---

![](/images/Stkoptions/Stockoptions.svg)
Black Scholes
Stock follows Geometric Brownian motion

Geometric Brownian motion -

$$ S_t = S_0 e^{\alpha t} $$
Share price S grows exponentially over time, \alpha is just a constant

Adding "Noise" to this exponential curve. This Noise is brownian motion
$$ S_t = S_0 e^{\alpha t + \beta B_{t}} $$ 
$$\beta $$ is a constant that is difficult to find.

$$ \frac{S_t}{S_0} = e^{\alpha t + \beta B_{T}} $$


Calculus requires smooth

Stochastic calculus has jagged randomness

S_t = S_0 e^{rt}

\frac{dS}{dt} = rS_0 e^{rt} = rS_{t}

Share price 
$$ \frac{S_t}{S_0} = e^{\alpha t + \beta B_{T}} $$
that $\beta B_{T}$ adds the "noise" to the exponential curve.
$$\frac{dS}{dt} = [r +  $$