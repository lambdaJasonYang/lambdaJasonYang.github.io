---
title: Recursion Scenarios
tags: mathcs, appliedmath, AI, stats
---

```plantuml
@startuml
digraph G {
 subgraph cluster_0{
f0[label=root]

 f1[label = <<s>if:base</s>>];

 f0-> f1 
 f0-> f2
  subgraph cluster_1{
f2[label = "else:rec"]
 

g1[label= <<s>if:base</s>>]
subgraph cluster_1{
g2[label ="else:rec"] 
 

g2 -> g3
h1 [label="if:base"]
g2 -> h1 [constraint=false]
subgraph cluster_2{
g3 [label="..."]
}
p3 [style=invis]
p3->h1 [style=invis]
}

}

  f2 -> g1 


f2 -> g2 
}
}
@enduml
```
> if:base node will only be used at a leaf node.

```python
def listprint(A,s,e):
    if s == e+1:
        return
    else:
        print(A[s]) #prints at each recursive stack
        return listprint(A,s+1,e)
print(listprint([2,3,4],0,2))
```

```python
def listprint(A,s,e):
    s = s
    e = e
    while s != e+1:
        print(A[s])
        s = s+1
    return
print(listprint([2,3,4],0,2))
```

# Recursion tree

Moving downwards is recursive call  
Moving horizontally is iterative loops  

Precedence: Downwards > Horizontal  
Recursive calls take priority over Iterative loops  
Recursive calls are naturally DFS  

```plantuml
@startuml
digraph G {
root [shape=circle]
subgraph cluster_0{
 A [shape=circle]
 B [shape=circle]
 C [shape=circle]
}
root -> A [color=red ]
root -> B [color=blue]
root -> C


subgraph cluster_1{
 subgraph cluster_2{
 A1 [shape=circle]
A2 [shape=circle]
A3 [shape=circle]
}
 B2 [label = ".." shape=rectangle]
 C3 [label=".." shape=rectangle]
 
}

A -> A1 [color=red]
A -> A2 [color=red]
A -> A3 [color=red]
A1 -> A [color=red]
A2 -> A [color=red]
A3 -> A [color=red]
A-> root [color=red]
B -> B2 [color=blue]
C-> C3
}
@enduml
```
# Examples

## return

```python
def loop(A):
    if len(A) == 1:
        print(A[0])
        return 8
    else:
        for i in range(0,len(A)):
            return loop([A[i]]) #BAD: EARLY REWIND 
        return 9
loop([2,3,4])
#>print 2
#>return 8
```
```python
def loop(A):
    if len(A) == 1:
        print(A[0])
        return 8 
    else:
        for i in range(0,len(A)):
            loop([A[i]]) #8 from leafnode .. does nothing
        return 9 
loop([2,3,4])
#>print 2 3 4
#>return 9
```
The loop doesn't work properly when we add a return statement. Why?   
Rather than DFS, it will hit the first deepest leaf it finds then returns back up to the root.  

```plantuml
@startuml
digraph G {
root [label="else: for i in range(0,len(A)):\n**loop([A[i]])**\nreturn 9" fontsize=8]

 A [label="if len(A) == 1:\nprint(A[0])\n**return 8**" fontsize=8]
 B [label="if len(A) == 1:\nprint(A[0])\n**return 8**" fontsize=8]
 C [label="if len(A) == 1:\nprint(A[0])\n**return 8**" fontsize=8]

root -> A [label=1]
root -> B [label=3]
root -> C [label=5]

A -> root [color=red label=2]
B -> root [color=red label=4]
C -> root [color=red label=6]


}
@enduml
```

## lazy recursion doesnt work

```python
def fact(x):
    if x == 1:
        return 1
    else:
        return lambda: x*fact(x-1)
print(fact(5)())
#>Error cant multiply 'int' with 'lambda'
#>fact :: () -> int
```

Possible alternative, an tail-recursive version

```python
def fact(x,a):
    if x == 1:
        return a
    else:
        return lambda: fact(x-1,x*a)
print(fact(5,1)()()()())
```

```python
def fact(x):
    if x == 1:
        return [1]
    else:
        return [x,fact(x-1)]
#>[5, [4, [3, [2, [1]]]]]
```


