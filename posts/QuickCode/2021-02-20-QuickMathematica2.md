---
title: Quick Mathematica Graphs
tags: prog, QuickCode
toc: y
---
# Building tree

## Example

```m
nn = Table[1+i,{i,1,3}]
(1->#)& /@ nn
Graph[(1->#)& /@ nn]
```
```
{2,3,4}
{1->2,1->3,1->4}
```

## Generalization

maps 1 to {2,3,4}

```m
nxt[src_,{dstmin_,dstmax_}] := (src->#)& /@ Table[i,{i,dstmin,dstmax}]
nxt[1,{2,4}]
{1->2,1->3,1->4}
minNode[depth_,b_] := Plus @@ Table[b^i,{i,0,depth}] 
minNode[2,3]
maxNode[depth_,b_] := Plus @@ Table[b^i,{i,0,depth-1}] + 1 
maxNode[2,3]
```

```m
Graph[Flatten[
 {
    nxt[1,3],
    nxt[2,{3^1,3^1 + 2}],
    nxt[3,{3^1+2,3^1 + 2 + 2}],
    nxt[4,{3^1+2+2,3^1 + 2 + 2 + 2}]
 }
    ],VertexLabels->Automatic]
```

```txt   
                        depth     Count    Max   Min   
        1                 0         1       1     na
     /  |   \      
   2    3       4         1         3       4      2
 /|\   /|\    / | \   
5 6 7 8 9 10 11 12 13     2         9       13     5
```

* $3^0 = 1$ is root . 0th depth is the root
    * if we started root at 0, we simply let $i=1$ aka omit $3^0$ in the summation in calculating min and max.
* $\sum\limits_{i=0}^n 3^n \in \{1,4,13..\}$ is the max node of the n-th layer. 
* $(\sum\limits_{i=0}^n 3^{n-1})+1 \in \{2,5,14..\}$

Getting children of 3

1. Get the index of 3, 3-Min=3-2 = 1
2. (Index*3)+(Min of next layer) = Starting value of children
   * (1*3)+(5) = 8
3. Starting value of children + (n-1) = Ending value of children
  * 8 + (3-1) = 10

| depth | Max=$\sum\limits_{i=0}^n 3^n$ | Min=$(\sum\limits_{i=0}^n 3^{n-1})+1$
| - | --- | --- |
| $0$ | $3^0=1$| na |
| $1$ | $3^0+3^1=4$ | $3^0+1=2$ |
| $2$ | $3^0+3^1+3^2=13$ | $3^0+3^1+1=5$
|$3$ |$3^0+3^1+3^2+3^3=40$ | $3^0+3^1+3^2+1=14$ |
