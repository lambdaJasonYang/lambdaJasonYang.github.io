---
title: CLRS Algos

tags: tech, prog, asm, C
---

### Problem Solving Heuristic

```plantuml
digraph G {

node [ranksep=0.2];

{rank="same"; A1 ; B1; D1; }
{rank="same"; A2 ; A3; D2;}

P1 [label = "problem"]

B1 [label = "Iterative"]
B2 [label = "Sublist\nLoop Invariant IH:[0..i]\n{[0..i],j..n}"]

A1 [label = "Recursive"]
C1 [label = "Strengthen problem"]
D1 [label = "Generative"]
D2 [label = "Tail-Rec State tree"]

A2 [label = "Split\nIH(fstHalf::list)\nIH(sndHalf::list)"]

A3 [label = "Atomic\nhead::element\nIH(tail::list) "]


P1 -> A1 [label="Determines\nIH"];
P1 -> B1;
A1 -> A2;
A1 -> A3;
A1 -> C1;
C1 -> P1 [label="Strengthen\nIH" constraint=false];
B1 -> B2;

P1 -> D1;
D1 -> D2;
}

```

##### Generative 

* Typically problems that ask to find all possible combinations that fulfils some constraint or target
* We create a generative tree of states using a tail recursive function

```python
mylist = input()
solutionSet = []
def func(target,buildsol,mylist...):
    if target == 0: #constraint satisfied
        solutionSet.append(buildsol)
    if target <= 0:  
        return #backtrack
    for i in mylist:
        func(target-i,buildsol+[i],mylist...)

```


### Examples




* problem: Sorting => IH: Sorted
  * Strengthen Problem : Quick Sort
    * Strengthen problem from Sorting to placing a chosen pivot in the correct sorted position
    * Must do this for all pivots 

  * Split : Mergesort
  * Atomic: BubbleSort
    * place head into correct position of IH(tail)


* problem: Longest Common Subsequence
  * Recursive, Atomic
  * 

```python
X = ["A","B","C","B","D","A","B"]
Y = ["B","D","C","A","B","A"]

m = len(X)+1 #
n = len(Y)+1 # 
b = [[ " " for row in range (0,n)] for col in range(0,m)] #
c = [[ -1 for row in range (0,n)] for col in range(0,m)] # 
for i in range(1,m):
    c[i][0] = 0
for j in range(0,n):
    c[0][j] = 0

for i in range(1,m):
    for j in range(1,n):
        if X[i-1] == Y[j-1]: # X[i] == Y[j] in CLRS
            c[i][j] = c[i-1][j-1]+1
            b[i][j] = "D"
        elif c[i-1][j] >= c[i][j-1]:
            c[i][j] = c[i-1][j]
            b[i][j] = "U"
        else:
            c[i][j] = c[i][j-1]
            b[i][j] = " "


def printMat(lst):
    for i in lst:
        print(i)

printMat(c)
# [0, 0, 0, 0, 0, 0, 0]
# [0, 0, 0, 0, 1, 1, 1]
# [0, 1, 1, 1, 1, 2, 2]
# [0, 1, 1, 2, 2, 2, 2]
# [0, 1, 1, 2, 2, 3, 3]
# [0, 1, 2, 2, 2, 3, 3]
# [0, 1, 2, 2, 3, 3, 4]
# [0, 1, 2, 2, 3, 4, 4]

printMat(b)
# [' ', ' ', ' ', ' ', ' ', ' ', ' ']
# [' ', 'U', 'U', 'U', 'D', ' ', 'D']
# [' ', 'D', ' ', ' ', 'U', 'D', ' ']
# [' ', 'U', 'U', 'D', ' ', 'U', 'U']
# [' ', 'D', 'U', 'U', 'U', 'D', ' ']
# [' ', 'U', 'D', 'U', 'U', 'U', 'U']
# [' ', 'U', 'U', 'U', 'D', 'U', 'D']
# [' ', 'D', 'U', 'U', 'U', 'D', 'U']
```

```python
def printLCS(b,X,i,j):
    if i == 0 or j == 0:
        return
    if b[i][j] == "D":
        printLCS(b,X,i-1,j-1)
        print(X[i-1]) #X[i] in CLRS
    elif b[i][j] == "U":
        printLCS(b,X,i-1,j)
    else:
        printLCS(b,X,i,j-1)
    
printLCS(b,X,len(X),len(Y))
#> BCBA
```



##### Insertion Sort

* Sublist [{0..i,j}...]
* IH : [0..i] is Sorted sublist
  * insert A[j] or Key into proper position in IH : [0..i]

```python
def InsertionSort(A):
    for j in range(1,len(A)):
        key = A[j]
        i = j - 1
        while i >= 0 and A[i] > key:
            A[i+1] = A[i]
            i = i - 1
        A[i+1] = key
A=[5,2,4,6,1,3]
InsertionSort(A)
print(A)
```