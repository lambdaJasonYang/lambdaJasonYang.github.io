---
title: PlantUML diagram list
tags: musings, prog
---


```plantuml
digraph g{
    graph [
rankdir = "LR"
];
"node1" [
label = "<f0> String| <f1> List | <f2> Set| <f3> Sorted Set | <f4> Hash"
shape = "record"
];

}
```

```plantuml
@startuml
digraph G {
  compound=true
rankdir="LR"
subgraph clusterb{
label = "Functor M";
subgraph clustera{
label = "Functor M";
 B [label="Int"]
};
}


subgraph cluster{
  label = "Functor M";
A [label="Int"]
}
B->A [label="join" ltail=clusterb lhead=cluster]


}
@enduml
```
```bash
@startuml
digraph G {
  compound=true
rankdir="LR"
subgraph clusterb{
label = "Functor M";
subgraph clustera{
label = "Functor M";
 B [label="Int"]
};
}


subgraph cluster{
  label = "Functor M";
A [label="Int"]
}
B->A [label="join" ltail=clusterb lhead=cluster]


}
@enduml
```


```bash
digraph g{
    graph [
rankdir = "LR"
];
"node1" [
label = "<f0> String| <f1> List | <f2> Set| <f3> Sorted Set | <f4> Hash"
shape = "record"
];

}
```






State Diagram  only have 1 type of arrow.
Activity Diagram has less control than state diagram.


```plantuml
@startuml
abstract        abstract
abstract class  "abstract class"
annotation      annotation
()              circle
class           class
<>              diamond
entity          entity
enum            enum
interface       interface
@enduml
```
```plantuml
@startuml

title Simple Object Diagram
interface mammal {
  -Void Breath
}
class Person {
  ..lifestyle..
  +Int Age
  +void Talk
  ..vitals..
  +Int Bloodpressure
}
mammal --o Person
@enduml
```

---

```plantuml
@startuml

1 -- 2 : solid
1 .. 3 : dashed
1 -[hidden]- 4 : hidden
1 ~~ 5 : dotted
1 == 6 : bold

7 -- 8
@enduml
```


---


### Sequence diagram
```plantuml
@startuml
participant User

User -> A: DoWork
activate A

A -> A:  Call Self

A -> B: << createRequest >>
activate B

B -> C: DoWork
activate C
C --> B: WorkDone
destroy C

B --> A: RequestCreated
deactivate B

A -> User: Done
deactivate A

@enduml
```

* Step into function call
  * `->`{.code}
* Step out/return from call
  * `-->`{.code}


```plantuml
@startuml

B -> B:
activate B
B -> B:
activate B
B -> B:
activate B
B --> B:
deactivate B
B --> B:
deactivate B
B --> B:
deactivate B
@enduml
```

```bash
@startuml
B -> B:
activate B
B -> B:
activate B
B -> B:
activate B
B --> B:
deactivate B
B --> B:
deactivate B
B --> B:
deactivate B
@enduml
```


```plantuml
@startuml
autoactivate on
B -> B:


B -> B:
B -> B:

return
return
return

@enduml
```
```bash
@startuml
autoactivate on
B -> B:
B -> B:
B -> B:
return
return
return

@enduml
```
```plantuml
@startuml
Alice -> Bob: Authentication Request
alt successful case
Bob -> Alice: Authentication Accepted
else some kind of failure
Bob -> Alice: Authentication Failure
group My own label
Alice -> Log : Log attack start
loop 1000 times
Alice -> Bob: DNS Attack
end
Alice -> Log : Log attack end
end
else Another type of failure
Bob -> Alice: Please repeat
end
@enduml
```

---

### graphviz
```plantuml
@startuml
digraph world {
size="7,7";
	{rank=same; 1 2;}
	{rank=same; 3 4;}
	{rank=same; 5 6;}
	{rank=same; 7 8;}
1 -> { 2 3 } [color=blue]
2 -> { 4 } [color=red dir=both]
4 -> { 5 6} [color=green dir=none comment=hello]
5 -> { 6} [color=green label="hello" style=dotted]
}
@enduml
```
```bash
@startuml
digraph world {
size="7,7";
	{rank=same; 1 2;}
	{rank=same; 3 4;}
	{rank=same; 5 6;}
	{rank=same; 7 8;}
1 -> 2 
2 -> 3 [color=blue]
}
@enduml
```


```plantuml
@startuml
digraph world {

node [shape=record];
struct1 [label="<f0> left|<f1> middle|<f2> right"];
struct2 [label="<f0> one| <f1> two"];

struct1:f0 -> struct2:f1;
}

@enduml
```

```bash
@startuml
digraph world {
node [shape=record];
struct1 [label="<f0> left|<f1> middle|<f2> right"];
struct2 [label="<f0> one| <f1> two"];

struct1:f0 -> struct2:f1;
}


@enduml
```

```plantuml
@startuml
digraph world {
rankdir=LR
node3 [shape=Mrecord, label="{ a | b | c }"]
}
@enduml
```
```bash
@startuml
digraph world {
rankdir=LR
node3 [shape=Mrecord, label="{ a | b | c }"]
}
@enduml
```
rankdir will turn the whole tree left, to right as well as "a b c"

---

### Mindmap

```plantuml
@startmindmap

* Solving \n Global \n Warming
 * Eating differently
  * Vegan
  * Vegetarian
  * Less processed foods
  * Buy local food
 * Travel
  * Bike more
  * Ride buses
  * Buy an electric car

left side
 * Home 
  *_ Energy audit
  *_ Use a cloths line
  *_ Add insulation
  *_ Get solar panels
 * Be a role model
  *_ Vote
  *_ Encourage others
  *_ Teach your kids

@endmindmap
```



```bash
@startmindmap

* Solving \n Global \n Warming
 * Eating differently
  * Vegan
  * Vegetarian
  * Less processed foods
  * Buy local food
 * Travel
  * Bike more
  * Ride buses
  * Buy an electric car

left side
 * Home
  *_ Energy audit
  *_ Use a cloths line
  *_ Add insulation
  *_ Get solar panels
 * Be a role model
  *_ Vote
  *_ Encourage others
  *_ Teach your kids

@endmindmap
```

---

### Activity diagram

```plantuml
@startuml
start
if (condition A) then (yes)
  :Text 1;
elseif (condition B) then (yes)
  :Text 2;
  stop
elseif (condition C) then (yes)
  :Text 3;
elseif (condition D) then (yes)
  :Text 4;
else (nothing)
  :Text else;
endif
stop
@enduml
```


---

### Arrow list

```plantuml
@startuml
left to right direction
skinparam nodesep 5
d3 ~~ v3 : ""~~""\n//dotted//
d2 .. v2 : ""..""\n//dashed//
d1 == v1 : ""==""\n//bold//
d0 -- v0 : ""--""\n//plain//


f13 --0 b13 : ""--0""
f12 --@ b12 : ""--@""
f11 --:|> b11 : ""--:|>""
f10 --||> b10 : ""--||>""
f9 --|> b9 : ""--|>""
f8 --^ b8 : ""--^ ""
f7 --\\ b7 : ""--\\\\""
f6 --# b6 : ""--# ""
f5 --+ b5 : ""--+ ""
f4 --o b4 : ""--o ""
f3 --* b3 : ""--* ""
f2 -->> b2 : ""-->>""
f1 --> b1 : ""--> ""
f0 -- b0 : ""-- ""


g10 0--0 r10 : "" 0--0 ""
g9 )--( r9 : "" )--(""
g8 0)--(0 r8 : "" 0)--(0""
g7 0)-- r7 : "" 0)-- ""
g6 -0)- r6 : "" -0)-\n ""
g5 -(0)- r5 : "" -(0)-\n""
g4 -(0- r4 : "" -(0-\n ""
g3 --(0 r3 : "" --(0 ""
g2 --( r2 : "" --( ""
g1 --0 r1 : "" --0 ""


@enduml

```