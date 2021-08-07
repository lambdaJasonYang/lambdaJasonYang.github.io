---
title: Language, Teaching and Set theory
tags: musings, mathcs
---

### The Problem
My first introduction to set theory, I remember being told:

"sets are sorta like bags but only loosely speaking"  
"sets can contain things"  

This cursed introduction gave me a misleading conceptualization of sets. 
Initially I had thought sets were similar to real objects such as bags or containers that contained elements.  
This physical conceptualization, lead me to believe(incorrectly) that elements could be taken or placed into sets at whim.  
This incorrect intuition caused a lot of trouble in understanding how sets contained everything that it specified WITHOUT EXCEPTION.   
So what are sets?

A set specifies a predicate.  
An element **MUST** be in a set if it satisify such predicate.  

How do we mitigate this confusion in teaching?  
From my own experience, most teachers would introduce/intialize a set by saying "Here we have a set that contains all even numbers"   

The problematic word **"Contains"** rears it's head.  
We must emphasize that sets are not objects like bags but **specifications**.


### Sets are Specifications

First we need to:  
STOP INITIALIZING sets by saying "Let S be the set that contains ..."  

example:  
1. DO NOT: "Let S be the set that contains red apple pies"  
2. DO:"Let S be the set that **specifies** red apple pies" 

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

