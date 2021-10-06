---
title: Translate Sequent Calculus to ND
tags: musings
---


3 types of systems: LK or Sequent Calc, ND and my custom.

Visual proof trees with the root being the conclusion.  
The leafs on top (P,Q,R,S) are the assumptions or $\Gamma$

```plantuml
@startuml
digraph world {
size="3,3";
	{rank=same; P Q R S;}
    { P Q R }-> T
    S -> U
    {T U } -> Conclusion
	
}
@enduml
``` 

Each Sequent is it's own proof tree. The rules tell us how to transform one tree to another.



ND:

elim-and

$$\frac{\Gamma \vdash A \land B  }{\Gamma \vdash A}$$

intro-and

$$\frac{\Gamma \vdash A \qquad \Pi \vdash B  }{\Gamma, \Pi \vdash A \land B}$$

* Given: Some set of assumptions $\Gamma$ can produce a proof for $A$
* Given:  another set of assumptions $\Pi$ can produce a proof for $B$.  
* Therefore: we can produce a proof for $A \land B$ by combining the two set of assumptions $\Gamma, \Pi$.

 


LK:

left-and

$$\frac{\Gamma , A,B  \vdash \Delta  }{\Gamma, A \land B \vdash \Delta}$$

right-and


$$\frac{\Gamma \vdash A, \Delta \qquad \Sigma \vdash B, \Pi  }{\Gamma, \Sigma \vdash \Delta, \Pi, A \land B}$$

#### LEM

ND:

$$\frac{\Gamma , A \vdash B \qquad \Pi , \neg A \vdash B  }{\Gamma, \Pi \vdash B}$$


custom:

$$\cfrac{\cfrac{[A \lor \neg A]^{lem}}{...}}{B} $$


#### Interlude

Notice that the right rules of LK, is very similar to the rules of ND.

#### Transformation

LK builds proof trees from bottom up using pattern matching.  
We want to prove

$$ \vdash \neg \neg A \rightarrow A $$

[look through LK table find  a rule that looks like $\cfrac{...}{... \vdash\ \fbox{}\rightarrow\fbox{}}$]   
[the right-implication rule matches $\cfrac{A,\Gamma \vdash \Delta, B}{\Gamma \vdash \Delta, A \rightarrow B}$ ]
