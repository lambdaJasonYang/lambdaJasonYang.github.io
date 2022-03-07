---
title: Quick Mathematica
tags: prog, QuickCode
toc: y
---

# Basics

| Concept | Python | Mathematica |
| --- | ----- | ----- |  
| list |`[1,2,3]` | `{1,2,3}`| 
| index | `a[0]` | `a[[0]]` |
| slice | `a[2:5]` | `a[[2;5]]` |
| list-notation | `[x**2 for x in range(10)]` |`Table[x^2, {x, 10}]` | 

## Solve for x
| Concept | Mathematica |
| --- | ----- | 
| Solve for x | `Solve[x+2y==5,x]`|
| Numerical Approx Solve | `NSolve[x^5 - 2 x + 3 == 0, x]` |
| Derivative | `D[x^2+2==0,x]` |
| Integrate | `Integrate[E^2x,x]` |

* output of `Solve` is a nested list in the form of Rules.
* use pattern matching `/.` to apply the rules

```m
sol = Solve[x+2y==5,x]
OUTPUT: {{x -> 5-2y}}
x /. sol
OUTPUT: 5-2y
```

## Differential Equation

| Concept | Mathematica |
| --- | ----- | 
| Analytical Solve | `DSolve[x]` |
| Numerical Solve | `NDSolve[x]` |


## Numbers

* `N` Reduce expr to numeric value
* `FixedPoint[x,7]` Find closest fixed point from x = 7 

## Map Fold

| Concept | Function | 
| --- | --- | 
| original | `f[3]` | 
| preapply | `f@3` | 
| postapply | `3 // f` |


| Concept | Function | 
| --- | --- |
| Map | `f /@ {1,2,3}` | 
| Fold | `NestList[,,]` | 

# Plot

## 2d+3d plots

```{.py group="p3" glabel="wolf"}
Plot[x^2,{x,0,30}]
```
```{.py group="p3" glabel="py"}
x = np.linspace(0,30,999)
f = lambda x: x**2
plt.plot(x,f(x))
```

```{.py group="p4" glabel="wolf"}
ContourPlot[1/(1+Exp[-(x1+x2)]),{x1,-Pi,Pi},{x2,-Pi,Pi}]
```
```{.py group="p4" glabel="py"}
x1 = np.linspace(-np.pi,np.pi,999)
x2 = np.linspace(-np.pi,np.pi,999)
f = lambda x1,x2: 1/(1+np.exp(-(x1+x2)))
X1,X2 = np.meshgrid(x1,x2)
plt.contourf(X1,X2,f(X1,X2))
```

```{.m group="p5" glabel="wolf"}
Plot3D[1/(1+Exp[-(x1+x2)]),{x1,-10,10},{x2,-10,10}]
```
```{.py group="p5" glabel="py"}
f=lambda x1,x2: 1/(1+np.exp(-(x1+x2)))
x1 = np.linspace(-10, 10, 99)
x2 = np.linspace(-10, 10, 99)
X1,X2 = np.meshgrid(x1, x2)
fig = plt.figure(figsize=(10, 6))
ax = fig.add_subplot(111, projection='3d')

ax.plot_surface(X1,X2,f(X1, X2),rstride=2, cstride=2,cmap=cm.jet,alpha=0.7,linewidth=0.25)
#ax.contour3D(X1,X2,f(X1, X2),rstride=2, cstride=2,cmap=cm.jet,alpha=0.7,linewidth=0.25)
#ax.plot_wireframe(X1,X2,f(X1, X2),rstride=2, cstride=2,cmap=cm.jet,alpha=0.7,linewidth=0.25)

plt.show()

```



## vector + plots

```{.m filename="annotated arrow"}
AArrow[{p_, q_}, label_:""] := {{Arrow[{p,q}], Inset[label,Midpoint[{p,q}]]}};
vec2[p_,q_,label_:""] := Graphics@AArrow[{p,q},label];
vec3[p_,q_,label_:""] := Graphics3D@AArrow[{p,q},label];
pnt[x_] := Graphics3D@Point[x]; 
```

* Combining vector arrow plots with equation plots

```{.m group="vp1" glabel="2d"}
v1 = {1,2};
v2 = {2,3};
oo = {0,0};
Show[vec2[oo,v1,"a"],
    vec2[oo,v2,"b"],
    vec2[v1,v2,"b-a"],
    Plot[x^2,{x,0,2}],
    
    ImageSize->Small,
    Axes->True
    ]
```

```{.m group="vp1" glabel="3d"}
vv1 = {1,1,5};
vv2 = {4,5,5};
ooo = {0,0,0};
Show[vec3[ooo,vv1,"v1"],
    vec3[ooo,vv2,"v2"],
    Plot3D[2*x1*x2,{x1,-1,1},{x2,-1,1}, PlotStyle->Opacity[0.3]],
    
    ImageSize->Small,
    Axes->True
    ]
```

<div>
<img alt="Output" src="data:image/png;base64,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">
</div>



### Annotated arrow



# AST 


Transform vec2 to vec3 functions in the Plot subsection above.

```m
vv1 = {1,1,10};
vv2 = {4,5,10};
ooo = {0,0,0};
vec2[p_,q_] := Graphics@Arrow[{p,q}]
vec3 := (vec2[#1,#2] /. Graphics -> Graphics3D)& 
```
`Graphics[stuff] /. Graphics -> Graphics3D` allows us to pattern-replace `Graphics` Head node with `Graphics3D` Head node in the AST

## print out AST
```mathematica
x*Tan[y + z] // TreeForm
```


# Nested list

```mathematica
NestList[f, x, 4]
(* {f[x],f^2[x],f^3[x],f^4[x]}*)


NestGraph[{f[#1], g[#1]} &, x, 3, VertexLabels -> All]
```

# functions

## Module 

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
## Lambda

* `#1, #2` are the bound variables of the lambda
* `&` indicates the expr is a lambda

```mathematica
f[x_,y_] := x*y + 2
f := (#1*#2 + 2)&
```



## Post apply and LateX output

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


# List 



# show all prob dist

```mathematica
StringCases[#, ___ ~~ "Distribution" ~~ ___ :> #] & /@ Names["System`*"]; 
DeleteCases[%, {}]
```



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