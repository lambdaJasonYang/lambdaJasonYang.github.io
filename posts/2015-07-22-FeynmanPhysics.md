---
title: Feynman Physics notes
tags: mathcs, physics, appliedmath
---

# (Inertial) frame of ref

* Motion/Force/Displacement is determined wrt to observer
* If Observer is not moving or is moving at constant velocity, this is an inertial frame
* Examples
  * Inertial frame: Subway at constant velocity, you can throw a ball and catch it normally
  * Non-Inertial frame: Subway accelerating, you throw a ball but it seems to move backwards.

## Ficticious force

It may be called "Ficticious" but it's a real measurable force.

* Centrifugal force pushing things outwards, falling back in seat when car accelerates
* Ficticious force only exist in non-inertial frames of reference.


# Newton's Second Law 

Solving the differential equation F=ma  
Solving for 1) distance function and 2) velocity function, each wrt time

1. 

$$m \frac{ds^2}{d^2t} = f$$

mass multiply by 2nd derivative of distance function s(t) wrt to time is force.  
Solve for the 2nd derivative gives us a closed form equation for the distance function s(t).  
The solution allows us to predict distance traveled given time,mass and force.

```mathematica
DSolve[m*s''[t] == f, s[t], t] // TeXForm
```
$$\left\{\left\{s(t)\to \frac{f}{m}\frac{1}{2}t^2+c_2 t+c_1\right\}\right\}$$
looks familiar
$s = \frac{1}{2} at^2 + v_0t$

2. 

$$m \frac{dv}{dt} = f$$

mass multiply by 1st derivative of velocity function v(t) wrt to time is force.  
Solve for the 1st derivative gives us a closed form equation for the velocity function v(t).  
This solution allows us to predict velocity traveled given time, mass and force.  

```mathematica
DSolve[m*v'[t] == f, v[t], t] // TeXForm
```

$$\left\{\left\{v(t)\to \frac{f t}{m}+c_1\right\}\right\}$$
looks familiar
$v_n=at+v_0$


---


# Volume 1

### Chapter 1

* very broad intro on states of matter and molecule

### 2 Basic Physics

* A charge creates an electromagnetic field
* Shaking a charge causes electromagnetic waves
  * different frequency causes different EM radiation

* Quantum Physics
  * Perpetual ambiguity of position and momentum of charges
  
### 6 Probability

### Chapter 22 Algebra



# Volume 3

