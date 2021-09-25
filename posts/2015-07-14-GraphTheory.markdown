---
title: Graph theory
tags: mathcs, appliedmath
---

### Cuts

* Cut : a partition of the NODES of a graph, NOT the edges.
* Cut-Set: The boundary edges that connects these two cuts or partitions. 
  * Elements of the cut-set are edges that exist in both Cuts bridging one cut to the other cut.

Obviously to Span a tree, one must choose at least 1 edge of a Cut-Set 

The minimum element in a Cut-Set is the edge contained in all MST.  


```plantuml
digraph G {
   
    
  subgraph cluster_0 {
    style=filled;
    color=lightgrey;
    node [style=filled,color=white];
    a0 -> a1 -> a2 [arrowhead=none];
    a3 -> a2 [arrowhead=none]; 
    label = "Cut A";
  }

  subgraph cluster_1 {
    node [style=filled];
    b0 -> b1 -> b2 -> b3 [arrowhead=none];
    label = "Cut B";
    color=blue
  }
  a3 -> b1 [arrowhead=none color=red];
  a1 -> b3 [arrowhead=none color=red];
  b2 -> a1 [arrowhead=none color=red];
  a3 -> a0 [arrowhead=none];


}
```

$${\color{red}\text{Cut-Set}}=\{e \in edges\ |\ e\ is\ {\color{red}RED}\}$$
$$Cut_A= \{a0,a1,a2,a3\}$$
$$Cut_B=\{b0,b1,b2,b3\}$$

### MST
