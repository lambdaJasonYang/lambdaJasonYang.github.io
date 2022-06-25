---
title: Chemistry
tags: mathcs, physics
---

>   A mole is just a number like pi

# VB Theory vs MO Theory

Atomic means single atom, Molecule means multiple combined atoms

* MO Theory
  * Atomic Orbitals combine to form Molecular Orbitals
  * Electrons can occupy the molecular orbitals classified  
  {Antibonding, Bonding}

* VB Theory
  * Molecular Orbitals do not exist 
  * Molecules consist of Atomic Orbitals and the INTERSECTION of Atomic orbitals classified   
    {Sigma-bond, Pi-Bond}

# MO Theory

Atomic orbitals combine to form Molecular Orbitals .  
Linear combination of atomic orbitals(LCAO) is used to calculate MO.

* INPUT: 
  * $\psi_A :: AtomicOrbital$ 
  * $\psi_B :: AtomicOrbital$  
* OUTPUT: 
  * $\psi_A + \psi_B=\psi_{bonding} :: MolecularOrbital$  
  * $\psi_A - \psi_B=\psi_{antibonding} :: MolecularOrbital$  

**Conservation of Orbitals**: If we input n number of Atomic Orbitals we will output n number of Molecular Orbitals.


$$\Psi = \text{e} ^{ \text{i}(kx- \omega t)}~~~(= \cos(kx- \omega t)+ \text{i} \sin(kx- \omega t)~)$$  
$$\dfrac{ \text{d} \Psi }{ \text{d} x}= \text{i}k \, \text{e} ^{ \text{i}(kx- \omega t)}= \text{i}k \Psi$$ 
$$\dfrac{ \text{d}^2 \Psi}{ \text{d}x^2}= \text{i}k \, \text{i}k \Psi= -k^2 \Psi$$ 
$$k= \dfrac{p}{ \hbar}~~~~~ \left ( [k]=~ \frac{kg \cdot \frac{m}{s}}{J \cdot s}= \frac{kg \cdot m}{Nm \cdot s^2}= \frac{kg }{kg \cdot \frac{m}{s^2} \cdot s^2}= \frac{1}{m}~ \right )$$ 
$$\dfrac{ \text{d}^2 \Psi}{ \text{d}x^2}= -\dfrac{p^2}{ \hbar^2} \Psi$$ 
$$- \hbar^2 \, \dfrac{ \text{d}^2 \Psi}{ \text{d}x^2}= p^2 \, \Psi$$ 
$$E= E_k+E_p= \frac{p^2}{2m}+E_p $$ 
$$E \Psi= \frac{p^2}{2m} \Psi+E_p \Psi=\frac{p^2 \Psi}{2m}+E_p \Psi=\dfrac{- \hbar^2}{2m} \, \dfrac{ \text{d}^2 \Psi}{ \text{d}x^2}+E_p \Psi \\$$


# Categorical chemistry?

* Objects are chemicals wrt protons
  * Isotopes are functors wrt neutrons
  * Ions are functors wrt electrons
