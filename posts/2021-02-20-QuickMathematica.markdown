---
title: Quick Mathematica
tags: prog, QuickCode
---

##### Post apply and LateX output

double backslash for post application

```mathematica
A ={{1,2,3},{-1,3,0}} // MatrixForm // TeXForm
A = TeXForm[MatrixForm[{{1,2,3},{-1,3,0}}]]
```

##### Simplify with condition

Simplify[expr,assum] : simjply with assumptions
Expand[expr,patt] : expand all but pattern patt
ExpandAll[expr] : expand product and exponents
ExpandAll[expr,patt] : expand when matching pattern patt

```mathematica
Assuming[Element[n, Integers] && n > 0,
   Integrate[Sin[n x]^2, {x, 0, Pi}]]
```


##### print out AST
```mathematica
x*Tan[y + z] // TreeForm
```

##### Functions
```
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



##### print out probability Distributions

```mathematica
StringCases[#, ___ ~~ "Distribution" ~~ ___ :> #] & /@ 
  Names["System`*"];
DeleteCases[%, {}]
```



##### Solve implicit differentiation

```mathematica
x[t_] := Cosh[t]
y[s_] := Cos[s]
z[x_, y_] := x[t] Exp[-y[s]]
D[z[x, y], s]

```

##### Taylor Series
```mathematica
Series[E^x, {x, 0, 7}]
```

---

##### Importing Fred Data
```mathematica
y = Import["C:\\Users\\User\\Downloads\\DCOILWTICO.csv", "Dataset", 
  "HeaderLines" -> 1]
DateListPlot[y]
```

##### Equity Timeseries Forecast
```mathematica
x = FinancialData["SPX", {{2020, 1, 1}, {2021, 1, 1}}]
tsm = TimeSeriesModelFit[x]
ListLinePlot[{tsm["TemporalData"], TimeSeriesForecast[x, {10}]}]
```

