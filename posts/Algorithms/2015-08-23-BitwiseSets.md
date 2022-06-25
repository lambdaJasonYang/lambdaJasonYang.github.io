---
title: Bitwise Operations and Sets
tags: mathcs, algorithms, puremath
---


* Extract: First take $n$ from singleton Set 
* Encode: Use n as exponent in Decimal representation $2^n$
* Convert: Convert base, $2^n :: \text{Decimal}$ to Binary representation

| Set | Encoding | Binary | Python |
| --- | --- | --- | --- |
| $\emptyset$ | $0$ | $00000$ | `0`{.python}|
| $\{0\}$ | $2^0=1$ | $00001$ | `0`{.python}|
| $\{1\}$ | $2^1=2$ | $00010$ | `0`{.python}|
| $\{2\}$ | $2^2=4$ | $00100$ | `0`{.python}|
| $\{3\}$ | $2^3=8$ | $01000$ | `0`{.python}|
| $\{4\}$ | $2^4=16$ | $10000$ | `0`{.python}|
| $\{0,1,2,3\}$ | $2^4 - 1=15$ | $01111$ | `0`{.python}|
| $\{3,4\}$ | $2^3 + 2^4=24$ | $11000$ | `0`{.python}|


$Set \rightarrow Encoding$  
$\{n\} \mapsto 2^n$  
$\{0,1,2,..n-1\} \mapsto 2^n-1$

Why do we take n from $\{n\}$ and Encode it by turning it into the exponent instead of simply converting to another base?  
