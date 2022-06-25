---
title: Interfaces, Class Composition, Diamond Problem
tags: OOP, prog
toc: y
---

* **Composition can be visualized as a tree**   
* **Composing classes can be thought of as embedding or inserting a class into another class**
  * "Embed a Engine class inside a Car class" == "Insert a Engine class inside a Car class"
  * Golang calls this struct embedding


# AbstractClass/Interface vs Composition

* Inheritance: AbstractClass/Interface are the subset of the group intersection of multiple classes
* Composition: Classes may be composed of the same component which is the group intersection of multiple classes 
* Notice how Composition and Inheritance are basically the same in functionality.


```plantuml
@startuml
interface has_engine
class "Toyota" as t2
class Toyota
class engine
Toyota --|> has_engine
t2 *-- engine
@enduml
```


<!--  -->

# Interface Composition

* $f(g(),h(i(),j()))$ 
$f$ is ReadAll and it has it's own type called ReadAll_attr along with composing $g$ and $h(i(),j())$ which are ReadRPC and ReadREST respectively.


```plantuml
@startuml
digraph G {
        {rank=same;5;6;}
        {rank=same; 2;3;}
        {rank=same; 4;1;7;}
	1 [label=ReadREST];
	2 [label=ReadXML];
	3 [label=ReadJSON]; 
        4 [label=ReadRPC];
        5 [label=ReadAll];
        6 [label=ReadAll_attr shape=box];
        7 [label=ReadREST_attr shape=box];
	1 -> 2;
        1 -> 3;
        5 -> 1;
        5 -> 4;
        6->5;
        7->1;
        
        
}
@enduml
```




```go
type ReadJSON interface {
    ...
}
type ReadXML interface {
    ...
}
type ReadREST interface {
    ReadREST_attr string
    ReadJSON
    ReadXML
}
type ReadRPC interface {
    ...
}
type ReadAll interface{
    ReadAll_attr string
    ReadRPC
    ReadREST
}
```

# Class composition

```go
type Wheel struct {
    ...
}
type Engine struct {

}
type Car struct {
    name string
    Engine
    Wheel
}
```

# Diamond problem

![](/images/OO/DiamondProblem.png)