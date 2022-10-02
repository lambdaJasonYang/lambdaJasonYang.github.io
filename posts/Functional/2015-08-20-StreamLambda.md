---
title: Implementing Stream with lambdas
tags: prog, mathcs, functional
---

```python
import numpy as np
import matplotlib.pyplot as plt

```

# Plot a recurrence relation 

$$ S_{t+1} = (1+r)\times S_t $$


## Naive (loop method)


```python
r = 0.025         # interest rate
T = 50            # end date
b = np.empty(T+1) # an empty NumPy array, to store all b_t
b[0] = 10         # initial balance

for t in range(T):
    b[t+1] = (1 + r) * b[t]

plt.plot(b, label='bank balance')
plt.plot(xdata,ydata)
plt.show()
```


    
![png](/images/2015-08-20-StreamLambda/output_3_0.png)
    


## Functional zipWith approach

$$[id, f, f\circ f, f\circ f\circ f .. f^n ]$$
$$ apply\ to$$
$$[10,10,10,10...]$$

* `funcPow`=$f^n$
* `zipWith` applys elementwise each function in the function-list with it's respective argument in the argument list.


```python
r = 0.025
S = lambda x: (1+r)*x
xdata = [i for i in range(1,51)]

def funcPow(n,f,x):
    if n == 0:
        return x
    else:
        return f(funcPow(n-1,f,x))

def zipWith(a,b):
    c = zip(a,b)
    return [(x[0])(x[1]) for x in c]
    

applylist = list(map(lambda n : (lambda x: funcPow(n,S,x)),xdata))
ones = [10]*50
flist = [applylist[i] for i in range(0,50)]

output = zipWith(flist,ones)
plt.plot(zipWith(flist,ones))
```




    [<matplotlib.lines.Line2D at 0x7fc9291d4ca0>]




    
![png](/images/2015-08-20-StreamLambda/output_5_1.png)
    


## LazyList method

$$[x, f(x), (f\circ f)(x), (f\circ f\circ f)(x), .. f^n(x)..f^\infty(x) ]$$

* lazylist needs a function and a base element which is 10

```python
r = 0.025
S = lambda x: (1+r)*x
class StreamLinkedList:
    def __init__(self,f,data):
        self.data = data
        self.next = lambda : StreamLinkedList(f,f(data)) 
    def __repr__(self):
        return str(self.data)
        
def take(n,xs):
    if n == 1:
        return [xs.data]
    else:
        IH = take(n-1,xs.next()) #take returns a list
        return [xs.data]+IH
a = StreamLinkedList(S,10)

gg=take(50,a)
plt.plot(xdata,gg)
```




    [<matplotlib.lines.Line2D at 0x7fc93c1b2d00>]




    
![png](/images/2015-08-20-StreamLambda/output_7_1.png)
    


