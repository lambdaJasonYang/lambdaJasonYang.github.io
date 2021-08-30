---
title: Proof technique Downward Induction
tags: mathcs
---

### Analogy
Building a number line with gaps with typical induction  
0 ⟶ 5 ⟶ 10 ⟶ 15 ⟶ (5n)...

Filling in these gaps with downward induction  
1 ⟵ 2 ⟵ 3 ⟵ 4 ⟵ 5  
6 ⟵ 7 ⟵ 8 ⟵ 9 ⟵ 10  
11 ⟵ 12 ⟵ 13 ⟵ 14 ⟵ 15  
...  
(k-4) ⟵ (k-3) ⟵ (k-2) ⟵ (k-1) ⟵ k

### Problem statement

Implement the identity function $id$:

* Upward induction on powers of 2  
  * $P(n) \rightarrow P(n^2)$   
* Downward induction by subtracting 1  
  * $P(n) \rightarrow P(n-1)$  

#### Overview

Find the least power of 2 greater than input n.  


#### Downward induction using dynamic programming

``` python
import math
#dynamic programming version
#upward induction by doubling, therefore each step is a power of 2
InNum = 17 #find the least power of 2 greater than 17, which will be the size of the dynamic programming array
#n = 2**(int(math.log(InNum,2))+1)
n = 999 #pretend n is infinity
idenA = [0]*(n+1)
idenA[0] = 0
idenA[1] = 1
k = 9
for i in range(0,k):                #[0  2  4  8  16  32  64]
    #example when i is 8 
    idenA[2**i] = 2**idenA[i] #upward induction, P(S) -> P(2S)
    #   P(2S) <= 2*P(S)
    for p in range((2**i),i+1,-1):   #[{base},{base},{4..2},{8..4},{16..8}] remember range(inclusive,excluded)

        idenA[p-1]= idenA[p]-1          #downward induction P(S) -> P(S-1)
        #   P(S-1)<=    P(S)-1
print(idenA[InNum])
#> 17
```
In an empty array, the upward induction is just filling up arrays indexed at 1,2,2^2,2^3 ...2^n

The downward induction is then filling in the gaps by looping downwards.

#### Downward induction using recursion
``` python
def is_integer_num(n):
    if isinstance(n, int):
        return True
    if isinstance(n, float):
        return n.is_integer()
    return False

#recursion version
import math
def idenB(p):
    def Up(g):
        #print("U")
        if g == 0:
            return 0
        if g == 1:
            return 1
        subprob = math.log(g,2)
        
        if is_integer_num(subprob):
            return 2**Up(subprob)   #P(S/2)->P(S) but recursively we move backwards
        else:
            return Down(g+1)    #P(S+1)->P(S) but recursively we move backwards
    def Down(v):
        #print("D")
        return Up(v)-1       #we can treat Up(v) as Down(g+1), therefore
        #same as return Down(g+1)-1
        #Down(g+1) represents identity of g+1, so to get the answer we must subtract 1 from it
    return Up(p)
idenB(6)
```
The example code will call Up and Down in a mutual recursive manner.

This will reduce n by 

> Recursion is informally the Computational Dual of Induction.  
> Induction tells us some chain of increasing P(n) is true.    
> Recursion uses that knowledge of induction to safely climb down that chain of truths to the base case.  
> -me  