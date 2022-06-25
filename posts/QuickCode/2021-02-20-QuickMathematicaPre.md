---
title: Quick Mathematica Utils
tags: prog, QuickCode
toc: y
---

```m
AArrow[{p_, q_}, label_:""] := {Inset[Style[label,Purple],Midpoint[{p,q}]], Arrow[{p,q}]};
vec2[p_] := Graphics@AArrow[{ConstantArray[0,Length[p]],p}];
vec2[p_,q_] := Which[  ArrayQ[q] == False, Graphics@AArrow[{ConstantArray[0,Length[p]],p},q],
                       ArrayQ[q] == True, Graphics@AArrow[{p,q},""] ]
vec2[p_,q_,label_] := Graphics@AArrow[{p,q},label];

vec[p_] := Which[ Length[p] == 2, vec2[p],
                  Length[p] == 3, vec2[p] /. Graphics -> Graphics3D ];
vec[p_,q_] := Which[ Length[p] == 2, vec2[p,q],
                     Length[p] == 3, vec2[p,q] /. Graphics -> Graphics3D ];
vec[p_,q_,label_] := Which[  Length[p] == 2,vec2[p,q,label],
                             Length[q] == 3,vec2[p,q,label] /. Graphics -> Graphics3D];
sty[c_:Red] := (# /. Arrow[x__] -> {c, Arrow[x]})&
(*  sty[Red,Dashed]@vec[{3,5}]*)

matrix[expr_] := expr /. List[p__]-> MatrixForm[List[p]]
cout[x__] := TeXForm[Row[{x}]];
pnt[x_] := Graphics3D@Point[x];
```

```m
angradian[u_,v_] = InverseFunction[Cos][(v.u)/(Norm[v]*Norm[u])];
anglebtw[x_,y_,l_:1,xbasis_:{1,0}] := Graphics@Circle[{0,0},l,{
                                            Min[angradian[x,xbasis],angradian[y,xbasis]], 
                                            Min[angradian[x,xbasis],angradian[y,xbasis]] + angradian[x,y]}]
```


* vec with 1 argument draws vector from origin
  * vec overloads 2D and 3D graphics

```m
Row[{
   Show[{
      vec[{3,3}],
      vec[{2,2}],
      vec[{2,2},{3,3},"hi"],
      anglebtw[{3,3},{2,2}]
   }]

}]
```

```m

```