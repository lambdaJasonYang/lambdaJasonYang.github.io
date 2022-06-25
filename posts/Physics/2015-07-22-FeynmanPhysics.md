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



# Circuits

* Voltage has no meaning without a reference point <- reason for grounding circuits
* Voltage only has meaning when you say the voltage **across a resistor**  
This means the potential difference before passing and after passing the resistor is the Voltage.  

## Series

* Voltage varies, Total voltage is sum of Voltage across all resistors
* Resistance varies, Total resistance is sum
* Current is same everywhere


## Parallel

* Voltage is same when passing each resistor in parallel
* Inverse(total resistance) = Inverse(sum resistors)
* Current is sum

# Ohm's law and Reality

## Real life power sources

How does Ohm's law play in reality?  

* In reality, electronics has a pre-determined static resistance.  
* Voltage is supplied by a power adapter which is semi-static since we get to choose the power adapter.  
* Voltage and current are linked.  
  * high voltage power supply, you get a high current.  
  * low voltage power supply, you get a low current.
<!--  -->
* A power adapter shows : 
  * INPUT: 100-120V
  * OUTPUT: 5V , 1.2A  
* This means the house power socket, (typically 120 Volts) is input into the power adapter and reduces the voltage.  
* The  1.2A just means the adapter maximum internals can only take 1.2 Amps 
  * This means if we use the power adapter on a very low resistance electronic device, the power adapter may break.
  * This DOES NOT mean the adapter *provides* current.  

 power adapter will provide different current to different electronic devices but it will always provide the same voltage.

 ## Battery doesnt follow ohm's law

 * Ohm's law only apply to resistors in a circuit
 * Battery is classified as a **Voltage Source** - a fixed voltage in a circuit
   * To find this fixed voltage : $E = E_Cathode - E_Anode$ where $E=Voltage$
   * Cathode: area of reduction reaction 
   * Anode: area of oxidation reaction

Chemical chart maps oxidation and reduction reaction to voltages.  
Use this to calculate total volts of a battery.  

 ## Does current or voltage kill

>  It isn't Volt that kills, it's current  
>  Standing on the cliff doesn't kill, it's the fall  

The above quote is an abuse of language, since Current is basically Voltage-in-action.    
