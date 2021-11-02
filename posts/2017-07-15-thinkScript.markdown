---
title: Thinkscript
tags: python, prog
---

* `def`{.bash} to instantiate variables
* Booleans are 1 or 0 , "yes" or "no" 


main default variables:

```bash
def a = open + high + low + close 
```
default variables have implicit index of current day $n$  

`open`{.bash} := $Open(n)$


#### Index

Index is relative to the current day $n$ in the negative direction.    
  
`volume[1]`{.bash} := $Volume(n-1)$

```bash
def diff = close - close[1]
#close - previous day close
```

#### Recursive

```bash
def data = data[1] + volume;
plot stuff = data;
```

$$ F_n = F_{n-1} + P(n) $$

* data := $F_n$
* data[1] := $F_{n-1}$
* volume := $P(n)$
* IH is cumulative volume at some arbitrary time

ToS assumes base case $F_0=0$

plot $n \mapsto F_n$  

The above will plot cumulative Volume over time. 

### Flow control

```bash
if 3 > 2 {
  def a = 44 
}
```

### plotting lower subgraph

```bash
declare lower;
c = 5;
plot outX = c;
```

### allow UI dropdown input

```bash
input a = close; #default value is close
plot outX = a;
```