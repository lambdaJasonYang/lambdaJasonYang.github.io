---
title: Quick Mathematica
tags: prog, QuickCode
toc: y
---

| Concept | Python | Mathematica |
| --- | ----- | ----- |  
| list |`[1,2,3]` | `{1,2,3}`| 
| index | `a[0]` | `a[[0]]` |
| slice | `a[2:5]` | `a[[2;5]]` |
| list-notation | `[x**2 for x in range(10)]` |`Table[x^2, {x, 10}]` | 

# Common functions
| Concept | Mathematica |
| --- | ----- | 
| Solve for x | `Solve[x+2y==5,x]`|
| Numerical Approx Solve | `NSolve[x^5 - 2 x + 3 == 0, x]` |
| Derivative | `D[x^2+2==0,x]` |
| Integrate | `Integrate[E^2x,x]` |

# Differential Equation

| Concept | Mathematica |
| --- | ----- | 
| Analytical Solve | `DSolve[x]` |
| Numerical Solve | `NDSolve[x]` |


# Numbers

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
| `f /@ {1,2,3}` | Map |
| `NestList[,,]` | Fold | 

# Plot

| Concept | python | Mathematica |
| - | ---- |  ---- |
|plot y=x^2 | `x = np.linspace(0,30,999)`</br>`f = lambda x: x**2`</br>` plt.plot(x,f(x))` | `Plot[x^2,{x,0,30}]`|
|Contour plot | `x1 = np.linspace(-np.pi,np.pi,999)` </br> `x2 = np.linspace(-np.pi,np.pi,999)` </br>`f = lambda x1,x2: 1/(1+np.exp(-(x1+x2)))` </br>`X1,X2 = np.meshgrid(x1,x2)` </br>`plt.contourf(X1,X2,f(X1,X2))` | `ContourPlot[1/(1+Exp[-(x1+x2)]),{x1,-Pi,Pi},{x2,-Pi,Pi}]` |
|plot3d | `f=lambda x1,x2: 1/(1+np.exp(-(x1+x2)))`</br>`x1 = np.linspace(-10, 10, 99)`</br>`x2 = np.linspace(-10, 10, 99)`</br>`X1,X2 = np.meshgrid(x1, x2)`</br>`fig = plt.figure(figsize=(10, 6))`</br>`ax = fig.add_subplot(111, projection='3d')`</br>`ax.plot_surface(X1,X2,f(X1, X2),rstride=2, cstride=2,cmap=cm.jet,alpha=0.7,linewidth=0.25)`</br>`plt.show()`</br> </br> we can `ax.contour3D` `ax.plot_wireframe` instead of `ax.plot_surface` | `Plot3D[1/(1+Exp[-(x1+x2)]),{x1,-10,10},{x2,-10,10}]` |



```mathematica
NestList[f, x, 4]
(* {f[x],f^2[x],f^3[x],f^4[x]}*)


NestGraph[{f[#1], g[#1]} &, x, 3, VertexLabels -> All]
```
# Module 

```bash
bleh = Module[
  {a = 1,
  b = 4},  
  a + b + 8]
```

```py
def blah():
  a = 1
  b = 4
  return a + b + 8
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