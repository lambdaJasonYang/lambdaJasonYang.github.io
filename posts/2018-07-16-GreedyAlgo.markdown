---
title: Proof - Greedy Algo
tags: mathcs, cs, math, algorithms, proofSeries
---
### Greedy Swap Proof
#### Example problem: 
$$ A = [a_1 , a_2, a_3 ...]$$
$$ O = [o_1, o_2, o_3 ...]$$
Each element of A represents a state.  
$$ Assume\ A \neq O $$ (NOTE THIS IS NOT A PROOF BY CONTRADICTION AS THERE MAY BE MULTIPLE OPTIMAL SOLUTIONS THAT ARE NOT EQUAL)
Therefore
$$ Either\ $$
$$\exists x \in O , x \notin A $$
$$ \exists x \in A, x \notin O$$
$$ a_p = o_q , a_q = o_p \ Swap$$


Prove by contradiction 

### Greedy Inductive Lookahead Proof
#### Example problem: A frog can take 1..k Steps each turn, get least number of turns to reach the n Step.

State = turn  
Greedy Solution, take the max k Step each State  

Proof of Correctness:
Show by induction that for all states, the current state is the most optimal  

Instinctively we believe that jumping farther in position is correlated to optimal least number of jumps.  

Denote $P(i,..)$ as the position function after $i$ steps.
$$P(i,..)\ \propto\ Optimality$$

Proving our naive Greedy algo $J$ will jump at least as far as some theoretical optimal algo $J^*$

Prove: $P(i+1,J^*) \leq P(i+1,J)$

IH: $P(i,J^*) \leq P(i,J)$

LEM: $P(i,J) \lt P(i+1,J^*) \lor  P(i,J) \geq P(i+1,J^*)$

```plantuml
@startuml
digraph world {
size="7,7";
	{rank=same; "P(i,J)" "P(i,J*)";}
    {rank=same; "P(i+1,J)" "P(i+1,J*)";}

"P(i,J*)" -> "P(i,J)"  [label = "IH"]
"P(i+1,J*)" -> "P(i+1,J)" [label = "?" style=dotted]
"P(i,J*)" -> "P(i+1,J*)" [label = "T"]
"P(i,J)" -> "P(i+1,J)" [label = "T"]
"P(i,J)" -> "P(i+1,J*)" [style = dashed label="lt LEM" arrowhead=empty]
"P(i+1,J*)" -> "P(i,J)" [style = dashed label="gte"]

}
@enduml
```

Arrow labeled "T" means obvious truth $P(i,J) \leq P(i+1,J)$.   
i to i+1 means you made a jump so you must be farther or at least the same position.

Dashed arrow labeled "lt" and "gte" comes from LEM which will help us through case analysis.  
Filled arrow head means unstrict inequality $\geq$  
Empty arrow head means strict inequality $\gt$  

We want to prove dotted arrow labeled "?".

PROOF: 

Using LEM will help us prove our goal.

* LEM Case "gte" $P(i,J) \geq P(i+1,J^*)$

```plantuml
@startuml
digraph world {
size="7,7";
	{rank=same; "P(i,J)" "P(i,J*)";}
    {rank=same; "P(i+1,J)" "P(i+1,J*)";}

"P(i,J*)" -> "P(i,J)"  [label = "IH"]
"P(i+1,J*)" -> "P(i+1,J)" [label = "?" style=dotted color="red"]
"P(i,J*)" -> "P(i+1,J*)" [label = "T"]
"P(i,J)" -> "P(i+1,J)" [label = "T" color="blue"]
"P(i+1,J*)" -> "P(i,J)" [style = dashed label="gte" color="blue"]

}
@enduml
```

Obvious from composition of inequalities:

Given: $P(i+1,J^*) \leq P(i,J)$ by "gte" case of LEM  
Given: $P(i,J) \leq P(i+1,J)$ by obvious truth   
Conclusion: $P(i+1,J^*) \leq P(i+1,J)$  
Goal solved for one case of LEM


* LEM Case "lt" $P(i,J) \lt P(i+1,J^*)$ :
```plantuml
@startuml
digraph world {
size="7,7";
	{rank=same; "P(i,J)" "P(i,J*)";}
    {rank=same; "P(i+1,J)" "P(i+1,J*)";}

"P(i,J*)" -> "P(i,J)"  [label = "IH"]
"P(i+1,J*)" -> "P(i+1,J)" [label = "?" style=dotted]
"P(i,J*)" -> "P(i+1,J*)" [label = "T"]
"P(i,J)" -> "P(i+1,J)" [label = "T"]
"P(i,J)" -> "P(i+1,J*)" [style = dashed label="lt" arrowhead=empty]


}
@enduml
```

Observation: Look at the maximum number of positions one can move in 1 transition.

Our greedy algo $J$ by definition will take r steps (by definition since it's greedy).    
No evidence $J^*$ the Theoretical optimal algo will take r steps.  

```plantuml
@startuml
digraph world {
rankdir = TB;
    {rank=same; "P(i,J)"; "P(i,J*)";}
    {rank=same; "P(i+1,J)"; "P(i+1,J*)";}
    {rank=sink; "P(i,J)+r"; "P(i,J*)+r";}

"P(i,J*)" -> "P(i,J)"  [label = "IH"]
"P(i+1,J*)" -> "P(i+1,J)" [label = "?" style=dotted]
"P(i,J*)" -> "P(i+1,J*)" [label = "T"]
"P(i,J)" -> "P(i+1,J)" [label = "T"]
"P(i,J)" -> "P(i+1,J*)" [style = dashed label="lt" arrowhead=empty]


"P(i,J)" -> "P(i,J)+r" [label="T"]
"P(i,J*)" -> "P(i,J*)+r" [label="T"]
"P(i,J*)+r" -> "P(i,J)+r" [label="IH+r"]

}
@enduml
```



PURPLE DOUBLE ARROW MEANS EQUALITY.  
Basically it says our greedy algo WILL take r steps.

```plantuml
@startuml
digraph world {

    {rank=same; "P(i,J)"; "P(i,J*)";}
    {rank=same; "P(i+1,J)"; "P(i+1,J*)";}
    {rank=sink; "P(i,J)+r"; "P(i,J*)+r";}

"P(i,J*)" -> "P(i,J)"  [label = "IH"]
"P(i+1,J*)" -> "P(i+1,J)" [label = "?" style=dotted color=red]
"P(i,J*)" -> "P(i+1,J*)" [label = "T"]
"P(i,J)" -> "P(i+1,J)" [label = "T"]
"P(i,J)" -> "P(i+1,J*)" [style = dashed label="lt" arrowhead=empty]


"P(i,J)" -> "P(i,J)+r" [label="T" ]
"P(i,J*)" -> "P(i,J*)+r" [label="T"]
"P(i,J*)+r" -> "P(i,J)+r" [label="IH+r" color=blue]

"P(i+1,J)" -> "P(i,J)+r" [label="Greed" dir=both color=purple style=bold constraint=false fontcolor=purple arrowHead="tee"]
"P(i+1,J)" -> "P(i,J)+r" [label="T"]
"P(i+1,J*)" -> "P(i,J*)+r" [label="T" color=blue]

}
@enduml
```
$\color{purple}{Greed: P(i+1,J)=P(i,J)+r}$

```plantuml
@startuml
digraph world {

    {rank=same; "P(i,J)"; "P(i,J*)";}
    {rank=same; "P(i+1,J)"; "P(i+1,J*)";}
    {rank=sink; "P(i,J)+r"; "P(i,J*)+r";}

"P(i,J*)" -> "P(i,J)"  [label = "IH"]


"P(i,J*)" -> "P(i+1,J*)" [label = "T"]
"P(i,J)" -> "P(i+1,J)" [label = "T"]
"P(i,J)" -> "P(i+1,J*)" [style = dashed label="lt" arrowhead=empty]


"P(i,J)" -> "P(i,J)+r" [label="T" ]
"P(i,J*)" -> "P(i,J*)+r" [label="T"]
"P(i,J*)+r" -> "P(i,J)+r" [label="IH+r" color=blue]

"P(i+1,J)" -> "P(i,J)+r" [label="Greed" dir=both color=purple style=bold fontcolor=purple arrowHead="tee"]
"P(i+1,J)" -> "P(i,J)+r" [label="T"]
"P(i+1,J*)" -> "P(i,J*)+r" [label="T" color=blue]
"P(i+1,J*)" -> "P(i,J)+r" [label = "?" style=dotted color=red]

}
@enduml
```

Notice how $P(i,J)+r$ behaves like a sink or a Terminal object.  
In general, finding terminal objects is a hint at optimality. 

* Both cases of LEM proved
  
By induction we shown our greedy algo will always be ahead in terms of position which is correlated to optimality.

remarks: Now that I look at it, LEM may not have been necessary but technically the proof is still correct. 