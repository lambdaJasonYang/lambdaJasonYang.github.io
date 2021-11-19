---
title: Dependency Injection, Design Patterns, OO vs FP
tags: prog
---

#### What are dependencies

Dependencies in software engineering terms means an object dependency. I'd like to extend that meaning to also include informational dependency.


This allows us to simply add an interface and the DI system will automatically initialize a class to fill that interface.  

What is code reflection? It's a form of meta-programming, where your code understands it's own code.   
An example is python `eval("print('hello')")`{.python} which can evaluate strings as code.  
ASP.NET Core uses reflection which is the only way it could swap a concrete class with an interface without the programmer's "consent".

---

#### Interfaces - Class is a subset relation

* Class Implements Interface
* Class Superset Interface

$$Interface \subset Class$$
$$INat \subset Nat$$

* Design an interface example
  * Given 4 classes: $Dog, Cat, Mouse, Human$.
  * $\underset{properties}{\bigcap} \{Dog, Cat, Mouse, Human\} = IMammel$

  $$IMammel \subset Dog$$ 
  $$IMammel \subset Cat$$
  $$...$$

Notice these are **strict subsets**.  
$IMammel \nsubseteq Dog$ since $Dog$ will always contain a constructor method new() which interfaces cannot have. 

---


#### Without DI

##### All Concrete classes depend on their constructor new() method  
This constructor dependency can be broken using DI.
  

```plantuml
digraph G {

subgraph cluster_a{
    Two [label="Nat" style=filled;];
    Three [label="Nat" style=filled;];
}

Plus [style=filled;];

INat [shape="box"];

newTwo [label = "new()::Nat" shape=plaintext]
newThree [label = "new()::Nat" shape=plaintext]

Two -> newTwo [label="depends" color=red]
Three -> newThree [label="depends" color=red]

Plus -> Two [label="depends"]
Plus -> Three [label="depends"]

Two -> INat [label="superset" style=dashed]
Three -> INat [label="superset" style=dashed]


}
```
> The hidden method constructor new()::Nat is also a dependency and colored in red


```python

def plus_OO():
    a = new()::Nat; #new() creates an Nat object one::Nat
    b = new()::Nat;
    return a + b
```

---

#### With DI


```plantuml
digraph G {

subgraph cluster_a{
    Two [label="INat" shape="box";];
    Three [label="INat" shape="box";];
    
}

Plus [style=filled;];


INatInjector [shape=doublecircle constraint=false];




Plus -> Two [label=depends]
Plus -> Three [label=depends]


INatInjector -> Two [label=injects style="dotted" constraint=false]
INatInjector -> Three [label=injects style="dotted" ]constraint=false


}
```

```python
INatInjector = new Injector(new()::Nat)

def plus_OO():    
    INat a; #Injector fills a with an object one::Nat
    INat b; #Injector fills b with an object one::Nat
    #INatInjector autofills the interfaces with concrete objects

    return a + b
```

---

### Compare OO vs FP

FP version

```python
def plus_FP(a::INat,b::INat):
    return a + b
```

* **FP does not have dependencies**  
* **we dont create objects in FP**  

Some might say that simply passing INat type arguments to a function is a form of dependency injection.  

The true difficulty of FP is modifying existing code base.   
Imagine we are given an exist chain of compositions.   
We need to modify add new customer data to type "Bool" and convert it to "F Bool"

Int --[f]--> Bool --[g]--> Float --[h]--> Maybe Int --[k]--> IO

* We must modify functions next to Bool: f, g
* What if we need to also output the additional data in "F Bool" to IO
  * We must modify every function downstream of "Bool": g,h,k to accept the extra data in "F Bool"
    * Similar to prop drilling in React

