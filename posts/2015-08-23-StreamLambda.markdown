---
title: Implementing Stream with lambdas
tags: prog, mathcs
---

Stream<int> = (int, lambda: Stream<int>)  

notice similarity to LinkedList:  
LinkedList<int> = (int, LinkedList)


Analogy:  

* Press a button and the next item will appear. 
* The button is implemented as an empty lambda.

``` python
class StreamLinkedList:
    def __init__(self,data):
        self.data = data
        self.next = lambda : StreamLinkedList(data*2) # empty lambda implementation acts like a button to get next value
        #if we exclude the lambda, we get StackOverflow by recursion
    def __repr__(self):
        return str(self.data)

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

