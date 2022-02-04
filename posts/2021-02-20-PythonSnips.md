---
title: To be Deleted
tags: prog, QuickCode
toc: y
---

# Functional


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



# Misconception

