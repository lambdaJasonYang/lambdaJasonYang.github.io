---
title: Generate Random Samples and Probability Distribution
tags: mathcs, appliedmath, physics
toc: y
---

[Probability playground](http://www.acsu.buffalo.edu/~adamcunn/probability/probability.html)

# Terms

$$P(X=\text{"it will rain today"})$$  
$X$ is the random variable  
$\mu$ mu is the mean  
$\sigma$ sigma is std  (z-score = sigma)
$\sigma^2$ sigma squared is variance

# Probability density function (pdf)

$$pdf(x;\mu , \sigma^2) = \frac{1}{\sqrt{2\pi \sigma^2}}exp({-\frac{1}{2}\frac{(x-\mu)^2}{\sigma^2}})$$

```{.py group="g1" glabel="scipy"}
import matplotlib.pyplot as plt
import numpy as np
import scipy.stats as stats

mu = 0
sigma = 1
x = np.linspace(mu - 3*sigma, mu + 3*sigma, 100)
y = stats.norm.pdf(x, mu, sigma) #stats lib pdf

plt.plot(x,y )
plt.show()
```

```{.py group="g1" glabel="manual"}

mu = 0 #mean
sigma = 1 #sigma-squared is variance
pdf = lambda x: (1/(np.sqrt(2*np.pi*sigma**2)))*(np.e**((-0.5*(mu-x)**2)/(sigma**2)))
x = np.linspace(mu - 3*sigma, mu + 3*sigma, 100)
y = list(map(pdf,x))
                                        
plt.plot(x,y)
plt.show()
```

![](/images/probTheory/pdf.svg)

# CDF: Integrate PDF 

* `CDF :: z-score -> percentile`  
  * in logistic regression we pretend `percentile` is `probability` 

$$cdf(x;\mu , \sigma^2) = \int_{-\infty}^{x} pdf(x;\mu , \sigma^2) \partial x $$

## CDF 

```python
import scipy.integrate as integrate
cdf = lambda x: integrate.quad(f,-np.inf,x)[0]
x = np.linspace(mu - 3*sigma, mu + 3*sigma, 100)
y =list(map(cdf,x))
plt.plot(x,y)
```

![](/images/probTheory/cdf.svg)


## 68-95-99 Rule

$$ 68 = \int_{-1\sigma}^{1\sigma}pdf(x;\mu , \sigma^2) \partial x $$
$$ 95 = \int_{-2\sigma}^{2\sigma}pdf(x;\mu , \sigma^2) \partial x $$
$$ 99 = \int_{-3\sigma}^{3\sigma}pdf(x;\mu , \sigma^2) \partial x $$
```python
std1 = integrate.quad(f,-1,1)
std2 = integrate.quad(f,-2,2)
std3 = integrate.quad(f,-3,3)
full = integrate.quad(f,-np.inf,np.inf)
print(std1,std2,std3,full)
```

```
(INTEGRAL         , ERROR-BOUND          )
(0.682689492137086, 7.579375928402476e-15) 
(0.9544997361036417, 1.8403560456416157e-11) 
(0.9973002039367399, 1.1072256503105314e-14) 
(0.9999999999999997, 1.017819138786878e-08)
```