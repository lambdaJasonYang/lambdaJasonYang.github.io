---
title: Very Long Lean - Dependent Type
tags: lean, mathcs, puremath ,categorytheory
toc: y
---

# Dependent Product

## Type Formation

$$\cfrac{\vdash X:Type \qquad x:X \vdash A(x):Type}{\vdash \prod\limits_{x:X} A(x) : Type }$$

## Term Introduction

$$\cfrac{ x:X \vdash a(x):A(X)}{\vdash \lambda x. a(x) : \prod\limits_{x':X} A(x') }$$


# Reading inductive types

```hs
inductive score : Type :=
| A : score
| B : score
| C : score

inductive Step : Score -> Score -> Prop :=
| * :.. -> Step A B
| % :.. -> Step B C
| o :.. -> Step A C
```


```txt
 Type Formation                              +----------------------------+
                                             |           Prop             |
                                             |                            |
                                             |                            |
+--------------+     +--------------+        |   +---------+              |
|   Type       |     |    Type      |        |   |Step A B |              |
|              |     |              |        |   |         |   +--------+ |
|              |     |              |        |   |     +   |   |Step B C| |
| +----------+ |     | +----------+ |        |   |  *      |   |        | |
| |  Score   | |     | |  Score   | |        |   |       & |   |   %    | |
| |        C | |     | |        B | |        |   +---------+   |        | |
| |A      -------------->  A    --------------->               |      + | |
| |     B    | |     | |     C    | |        |                 |  ^     | |
| +----------+ |     | +----------+ |        |    +----------+ |        | |
|              |     |              |        |    | Step A C | +--------+ |
+--------------+     +--------------+        |    |          |            |
                                             |    |  *   o   |            |
                                             |    +----------+            |
                                             |                            |
                                             +----------------------------+
```

Term introduction is us selecting one of those terms `* + ^` inside a `Step _ _` set