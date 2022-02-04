---
title: Quick Mathematica
tags: prog, QuickCode
toc: y
---

# Common functions
* `D[expr,x]` Derivative
* `Integrate[expr,x]` 
* `DSolve[x]` Analytically Solve diffyq
* `NDSolve[x]` Numerically Solve diffyq
* `N` Reduce expr to numeric value
* `FixedPoint[x,7]` Find closest fixed point from x = 7 

| Function | Detail |
| --- | --- |
| f[3] | original |
| f@3 | preapply |
| 3 // f | postapply |

# Map Fold

| Function | Detail |
| --- | --- |
| `/@` | Map |
| `NestList[,,]` | Fold | 


```mathematica
mapf := f /@ {1,2,3}
(* {1,4,9} *)
```

`NestList[function,initial-value, number-of-function-app]`

```mathematica
NestList[f, x, 4]
(* {f[x],f^2[x],f^3[x],f^4[x]}*)


NestGraph[{f[#1], g[#1]} &, x, 3, VertexLabels -> All]
```

## Table

Table is basically a map.

```mathematica
Table[f,{x,0,10}]
(* {f[0],f[1],...*)
```



# Lambda

* `#1, #2` are the bound variables of the lambda
* `&` indicates the expr is a lambda

```mathematica
f[x_,y_] := x*y + 2
f := (#1*#2 + 2)&
```



# Post apply and LateX output

double backslash for post application

```mathematica
A ={{1,2,3},{-1,3,0}} // MatrixForm // TeXForm
A = TeXForm[MatrixForm[{{1,2,3},{-1,3,0}}]]
```

# Simplify with condition

Simplify[expr,assum] : simjply with assumptions
Expand[expr,patt] : expand all but pattern patt
ExpandAll[expr] : expand product and exponents
ExpandAll[expr,patt] : expand when matching pattern patt

```mathematica
Assuming[Element[n, Integers] && n > 0,
   Integrate[Sin[n x]^2, {x, 0, Pi}]]
```


# print out AST
```mathematica
x*Tan[y + z] // TreeForm
```

# List 



# show all prob dist

```mathematica
StringCases[#, ___ ~~ "Distribution" ~~ ___ :> #] & /@ Names["System`*"]; 
DeleteCases[%, {}]
```


# Module
```mathematica
F[x_] = x*2+1
F[x_] = Module[
  {a,b},
  a=2;
  b=1;
  x*a+b
]
```

x_ the underscore means function arg  
Module allows a local env  



# print out probability Distributions

```mathematica
StringCases[#, ___ ~~ "Distribution" ~~ ___ :> #] & /@ 
  Names["System`*"];
DeleteCases[%, {}]
```



# Solve implicit differentiation

```mathematica
x[t_] := Cosh[t]
y[s_] := Cos[s]
z[x_, y_] := x[t] Exp[-y[s]]
D[z[x, y], s]

```

# Taylor Series
```mathematica
Series[E^x, {x, 0, 7}]
```

---

# Importing Fred Data
```mathematica
y = Import["C:\\Users\\User\\Downloads\\DCOILWTICO.csv", "Dataset", 
  "HeaderLines" -> 1]
DateListPlot[y]
```

# Equity Timeseries Forecast
```mathematica
x = FinancialData["SPX", {{2020, 1, 1}, {2021, 1, 1}}]
tsm = TimeSeriesModelFit[x]
ListLinePlot[{tsm["TemporalData"], TimeSeriesForecast[x, {10}]}]
```

---

# Visualize network related keywords

```mathematica
docCrawler[startSymbol_, depth_] := 
  Flatten[Rest[
    NestList[
     Union[Flatten[
        Thread[# -> 
            CommonName[
             Entity["WolframLanguageSymbol", #][
              "RelatedSymbols"]]] & /@ Last /@ #]] &, {"" -> 
       startSymbol}, depth]]];
edges = docCrawler["ProbabilityDistribution", 3];
Graph[edges, VertexLabels -> "Name", ImageSize -> 1200]
```