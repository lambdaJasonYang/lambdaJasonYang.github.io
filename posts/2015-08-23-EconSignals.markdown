---
title: Econ terms
tags: musings
---

Oct. 1 to Sept. 30

Fed saying they will lower bank's (interest rate/federal funds rates/ nominal rate)-> increase money supply -> inflation
Fed's interest rate is inversely proportional to money multiplier.


Raising debt limit

Rising bond yield hurts the SP500
Bond yield = Risk free interest rate

Higher risk free interest rate lowers relative values of equity future earnings

Federal reserve chair saying "Inflation" will hurt equities



Treasury bond price inverse to Yield

* Full green arrow means when A goes up, B goes Up
* Half red-green means  when A goes down, B goes Up.
* Half green-red means  when A goes up, B goes down.
* Note (red-green) and (green-red) are interchangeable.

Composition of arrows : multiply weight of edges 
 
* red-green arrows = (-1) weight edges 
* Full green arrow = (1) weight edges
* Example: how does A affect C?
  * A --red-green-> B --red-green-> C
  * A ---(-1)---> B ---(-1)---> C
  * A ---(1)---> C
  * A ---green---> C

Red nodes indicate news events or one-off catalysts.

Treasury bond Interest Rate is not same as Federal funds interest rate.
When news say Fed is lowering interest rates, them mean fed funds rate.

```plantuml
@startuml
digraph G {
   
  1 [label="DiscountRate\nFedFundsRate\nInterestRate"];
  2 [label="money\nmultiplier" ];
  3 [label="money supply" ];
  1 -> 2 [color="red:green;0.5"];
  2 -> 3 [color=green];
  
  4 [label="bond price"];
  5 [label="bond yield,\n risk-free-rate"];
  6 [label="SP500"];
  7 [label="QE\nBond Buyback"];
  4 -> 5 [color="green:red;0.5"];
  5 -> 6 [color="green:red;0.5"];
  7 -> 4 [color="green"];
  4 -> 6 [style="dashed" color="green"];
  7 -> 3 [style="dashed" color="green" ];
  8 [label="Debt Limit\nConflict" color="red"];
  8 -> 6 [color="red"];
  1 -> 5 [color="green"];
  1 -> 6 [style="dashed" color="red:green;0.5"];

}
@enduml
```