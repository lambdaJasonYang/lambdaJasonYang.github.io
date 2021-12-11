---
title: Python Snippets
tags: prog, QuickCode
toc: y
---

# Functional

## Lambda 

```go
f := func() interface{}{return 2}
fmt.Println(f())
//output 2
fmt.Println((func() interface{}{return 2})())
//output 2
```

## Lazy Streams

``` python
class StreamLinkedList:
    def __init__(self,data):
        self.data = data
        self.next = lambda : StreamLinkedList(data*2) # empty lambda implementation acts like a button to get next value
        #if we exclude the lambda, we get StackOverflow by recursion
    def __repr__(self):
        return str(self.data)
def head(k):
    return k.data
def tail(k):
    return k.next()
def take(n,s):
    if n == 0:
        return []
    else:
        return [head(s)] + take(n-1,tail(s)) 
    
def reclist(n,initList):
    if n == 1:
        return [initList]
    else:
        return reclist(n-1,initList.next()) + [initList]

a = StreamLinkedList(3)
print(a)
#>3
b = a.next()
print(b)
#>6
c = a.next().next()
print(c)
#>12
print(reclist(4,a))
#>[24, 12, 6, 3]
```


## Tree

* Golang does not allow recursive data structs, 2 ways to solve this:
  * Self-reference to self wrapped in a Functor
    * Tree self-refers to ListFunctor(Tree) 
  * Use pointer to self
   

```go
type tree struct{
    data int
    left []tree //tree wrapped in List functor
    right *tree //pointer to self
  }

   a := tree{data:2,left: []tree{tree{data:4}}}
   fmt.Println(a.left[0].data)
   b := tree{data:2,right: &tree{data:4}}
   fmt.Println(b.right.data)
```


# Misconception

