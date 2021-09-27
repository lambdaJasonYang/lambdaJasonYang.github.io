---
title: Quick Mathematica
tags: prog, QuickCode
---

##### print out probability Distributions

```mathematica
StringCases[#, ___ ~~ "Distribution" ~~ ___ :> #] & /@ 
  Names["System`*"];
DeleteCases[%, {}]
```


##### Simplify with condition

```mathematica
Assuming[Element[n, Integers] && n > 0,
   Integrate[Sin[n x]^2, {x, 0, Pi}]]
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