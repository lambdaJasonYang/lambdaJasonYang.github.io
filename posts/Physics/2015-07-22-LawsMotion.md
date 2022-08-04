---
title: Derive kinematics + motion w/ calculus
tags: mathcs, physics, appliedmath
---

Given Position function $s(t)$  

Deriving Velocity function

$$ s'(t) = \frac{ds}{dt} = v(t)$$

Deriving Acceleration function

$$ s''(t) = v'(t) = \frac{dv}{dt} = a(t)$$

# 1st equation of kinematics

Given constant acceleration and initial velocity, find the find final velocity    

Notice how this doesnt work `DSolve[{s''[t]==-9.81, s'[0]==0},s'[t], t]`    
but this does `DSolve[{v'[t]==-9.81, v[0]==0},v[t], t]  `

$$\{\{v(t)\to -9.81 (1. t+0.)\}\}$$

# 2nd equation of kinematics

Given constant acceleration = 13, derive equation for position 

`DSolve[s''[t]==13, s[t], t] // TeXForm`  
$$\left\{\left\{s(t)\to \frac{13 t^2}{2}+c_2 t+c_1\right\}\right\}$$  


# 3rd equation of kinematics

`Solve[{a==((v[t]-v[0])/t),s==((1/2)(v[t]+v[0])*t)},{v[t],v[0]}] // TeXForm`  

$$\left\{\left\{v(t)\to \frac{a t}{2}+\frac{s}{t},v(0)\to -\frac{a t^2-2 s}{2 t}\right\}\right\}$$
  

`Simplify[v[t]^2 - v[0]^2  /. Solve[{a==((v[t]-v[0])/t),s==((1/2)(v[t]+v[0])*t)},{v[t],v[0]}]] // TeXForm`  

$${2𝑎𝑠}$$


# Newton's Second Law 

Solving the differential equation F=ma  
Solving for 1) distance function and 2) velocity function, each wrt time

1. 

$$m \frac{ds^2}{d^2t} = f$$

```mathematica
DSolve[m*s''[t] == f, s[t], t] // TeXForm
```
$$\left\{\left\{s(t)\to \frac{f}{m}\frac{1}{2}t^2+c_2 t+c_1\right\}\right\}$$
looks familiar
$s = \frac{1}{2} at^2 + v_0t$

2. 

$$m \frac{dv}{dt} = f$$

```mathematica
DSolve[m*v'[t] == f, v[t], t] // TeXForm
```

$$\left\{\left\{v(t)\to \frac{f t}{m}+c_1\right\}\right\}$$
looks familiar
$v_n=at+v_0$

