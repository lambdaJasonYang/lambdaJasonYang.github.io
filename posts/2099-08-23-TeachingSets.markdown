---
title: Language, Teaching and Set theory
tags: musings, mathcs
---

### The Problem
I remember being told:

"sets are sorta like bags but only loosely speaking"  
"sets can contain things"  

This introduction to sets gave me the incorrect intuition of what sets were and how they behave. 
It lead me to believe sets were behaved as bags or containers that contained elements.  
This physical conceptualization, lead me to believe(incorrectly) that elements could be taken or placed into sets at whim. 
This misunderstanding caused a lot of trouble in understanding proofs.
So without futher ado.   
what are sets?

A set specifies a predicate.  
An element **MUST** be in a set if it satisify such predicate.  

If we really wanted to use the bag and container analogy, then we would have to believe this bag had some magical vortex that sucked every element that the predicate satisfies inside the bag.

Moving on, let's get to the bottom of why I had such a misunderstanding of how sets worked.  
From my own experience, most teachers would introduce/intialize a set by saying "Here we have a set that contains all even numbers"   

The problematic word **"Contains"** rears it's head.  
We must emphasize that sets are not objects like bags but **specifications**.


### Sets are Specifications

First we need to:  
STOP INITIALIZING sets by saying "Let S be the set that contains ..."  

example:  
1. DO NOT: "Let S be the set that contains apple pies that are tasty"  
2. DO:"Let S be the set that **specifies** **ALL** red apple pies that are tasty" 

Now instead of focusing on the set S as it's own object, I am now focusing on the elements that satisfies the specification which is really what sets were meant for.

### Stop using Contains
After introducing the set, we prod the set with elementhood $\in$ and some element $x$.  
We may ask "is $x$ in the set?" or assert "$x$ is in the set".

"apple pie $x$ is in set S"  
$$ x \in S $$

Isnt "apple pie $x$ is in set S" and   
"set S contains apple pie $x$" the same thing?  


| Syntax 1    | Syntax 2    | Syntax 3    |
| :---------: | :---------: | :---------: |
|apple pie $x$| is in       | Set S       |
| $x$         | $\in$       |  S          |

Yes, semantically overall, they are equivalent but the first statement is closer to the mathematical notation in terms of discrete syntax and semantic ordering (as shown in the chart above).

Again, there is the issue that "contains" implies that sets have some physical manifestation.

Therefore, it is preferable to say "apple pie $x$ is in set S" over   
"set S contains apple pie $x$".

example:  
DO NOT: "$\mathbb{N}$ contains 1,2,3"  
DO: "1,2,3 are in $\mathbb{N}$"  
 

##### Aside
Perhaps after students have a better grasp on sets, we can ease up on the "contains" restriction in terms of elementhood   
but I believe initializing sets with "specifies" should become the standard as it explicitly indicates first use of the set to the readers.

