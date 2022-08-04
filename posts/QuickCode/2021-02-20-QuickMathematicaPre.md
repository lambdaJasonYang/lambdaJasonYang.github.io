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
sty[c__:Red] := (# /. Arrow[x__] -> {c, Arrow[x]})&
(*  sty[Red]@vec[{3,5}]*)

matrix[expr_] := expr /. List[p__]-> MatrixForm[List[p]]
cout[stmt__] := TeXForm[Row[{stmt}]];
pnt[x_] := Graphics3D@Point[x];

(* polynomial *)
(* helper functions *)
 vars[n_, m_] := Flatten@Transpose[Outer[Symbol@StringJoin[##] &, CharacterRange["A", "Z"][[;; m]], ToString /@ Range[n]]]
 polyvar[v_] :=  Flatten[{1,vars[v-1,1]}]; 

(* Give a list of coefficents and it will generate a polynomial with variables *)
poly[coef_] := Transpose[coef].polyvar[Length@coef];
poly@Thread[{{1,2,3}}];

T := Transpose;
Dim = Dimensions;
Ones[n_] := ConstantArray[1,n]
addCol[x_] := MapThread[Append, {#, x}] &
(*  addCol[ConstantArray[1,2]]@{{1,3},{3,4}} // matrix*)
addRow[x_] := Append[#,x]&

unwrap := #[[1]][[1]]&
(* unwrap[{{x+1}}] = x+1 *)

(* 3d gradient color *)
opt3d[opacity_:1] := {PlotStyle->Opacity[opacity],ColorFunction -> Function[{x, y, z}, Hue[z]]};
(* Plot3D[f,{x1,0,1},{x2,0,1},Evaluate@optcolor3d] *)
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