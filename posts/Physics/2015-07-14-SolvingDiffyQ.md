---
title: Solving Differential Equations
tags: mathcs, appliedmath, physics
---

### Differential Equations

Properties

* Order: order of highest derivative
* (Non)Linear: 
* (ODE)PDE
* Homogeneity

---

##### Initial Value Problems

notation: 
$y' := \frac{dy}{dx}$
$y := y(x)$

Given Differential Equation $$y'' - 5y' + 6y = 0$$  
Solve for set of equations that satisfy Differential equation:

```mathematica
DSolve[y''[x] - 5*y'[x] + 6 y[x] == 0, y[x], x]
```


$$\left\{\left\{y(x)\to c_1 e^{2 x}+c_2 e^{3 x}\right\}\right\}$$

Given Initial conditions:
$$y(0)=3$$
$$y'(0)=1$$
Solve by finding the specific functions that satisfy the initial condition and the given differential equation

* $y(0)=3$
  * Plug into previous solution $y(x)=c_1 e^{2 x}+c_2 e^{3 x}$
  * $y(0)=c_1 +c_2 = 3$
* $y'(0)=2$
  * Find derivative of previous solution $y(x)=c_1 e^{2 x}+c_2 e^{3 x}$
    * $y'(x) = 2c_1 + 3c_2$
  * $y'(0)=2c_1 + 3c_2 = 1$  


Solve system of linear equations for constants $c_1$,$c_2$

$$
\begin{aligned}
    c_1 + c_2  &=  3\\
    2c_1 + 3c_2 &= 1 
  \end{aligned}
  \quad\Longleftrightarrow\quad
  \begin{bmatrix}
 1 & 1 \\
 2 & 3 \\
\end{bmatrix}
\begin{bmatrix}
 c_1 \\
 c_2 \\
\end{bmatrix}=
\begin{bmatrix}
 \text{3} \\
 \text{1} \\
\end{bmatrix} $$

```mathematica
mat = {{1, 1}, {2, 3}}
vars = {{c1}, {c2}}
b = {{3}, {1}}

Solve[{mat . vars == b}, {c1, c2}]
```

$$\{\{c_1\to 8,c_2\to -5\}\}$$

Solution: $$y = 8e^{2x}-5e^{3x}$$

---

### Examples

##### Radioactive Decay , substance amount y[t]

$$\frac{dy}{dt} = -ky(t)$$
 

```mathematica
DSolve[y'[t] == -k*y[t], y[t], t] // TeXForm
```
$$\left\{\left\{y(t)\to c_1 e^{-k t}\right\}\right\}$$

$$y(0)=k$$

---


---

##### Uniqueness and Existence Problems

Theorem:  
Given f(x,y) continous 