---
title: Energy Based Models
tags: mathcs, AI
---

# Manifold 

* Manifold is a topological space that locally resembles a euclidean space
  * topological manifold
  * differentiable manifold
  * riemannian manifold

## Embedding

* Injective function from one manifold to another that preserves structure

## Example

* We have a dataset of 3d faces
  *

### Energy Based Model vs Probablistic Model
Probablistic Model are based on a distribution derived from the possible solution set.   
What if the solution set is intractable or infinite?  


Probablistic Model result in extremely steep gradients, more like fractured crevices where the solution is at the bottom and everywhere else is infinitely wrong. There is no nuance to the model.    
EBM have much smoother gradients.  

### Energy Based Model vs regular Neural nets (Training and Inference)
Regular Neural nets predict a single output.  
Energy Based Model predicts a set of plausible outputs.   

It is possible to convert EBM to probablistic model using Boltzmann probability distribution.  

Unlike NN, training EBM is basically just building the energy map.
How do we train EBM?
Most methods say we give our dataset the lowest energy level. 
There are different ways to design the high energy space. 
Example include creating data we know is definitely incorrect and giving it a high energy.  
Example2 include giving high energy to all data other than our dataset (WARNING, this is basically the same as a probabilistic model with the issue of extremely steep gradients)


Two types of inference architectures.  
Latent energy variable.   

$$P(y|x) = \frac{e^{-\beta E(x,y)}}{\int_{y'}e^{-\beta E(x,y')}} $$

