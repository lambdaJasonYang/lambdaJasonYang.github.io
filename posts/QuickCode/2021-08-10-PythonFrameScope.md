---
title: Frames Scope and Python
tags: prog
---


# Meta-Classes

* Even Class declarations are objects which are instantiated by Meta-classes
  * Meta-classes construct Classes
  * Classes construct Objects

```py
def chooseclass(name):
    class Foo(object):
        pass
    return Foo
theclass = chooseclass("asd")
theobj = theclass()

```

```text
   Frame           Objects
Global Frame
chooseclass -----> function chooseclass(name)
theclass    -----> Foo class
theobj      -----> Foo instance
```

# List and Ref

```py
a = 1 # create int(1) literal, bind it to a in Global Frame
b = a # bind b to the same literal that a binds to
a = 2 # create int(2) literal, bind it to a in Global Frame
```

```txt
  Frame       Objects
Global Frame
   a  2
   b  1
```

The above code does not create any reference to any objects.

Python names work like pointers with automatic de/referencing but do not allow explicit pointer operations. Other targets represent indirections, which behave similar to pointers.


```py
i = 5  # name `i` refers to object `5`
j = i  # ???
j = 3  # name `j` refers to object `3`
```

```C
int three=3, five=5;  // objects
int *i, *j;           // names
i = &five;   // name `i` refers to position of object `5`
j = i;       // name `j` refers to referent of `i`
j = &three;  // name `j` refers to position of object `3`
```


```py
i = [1,2,3]  # name `i` refers to object `[1, 2, 3]`
j = i        # name `j` refers to referent of `i`
i[0] = 5     # ???
```
```C
typedef struct{
    int *elements[3];
} list;  // length 3 `list` type

int one = 1, two = 2, three = 3, five = 5;
list values = {&one, &two, &three};  // objects
list *i, *j;                         // names
i = &values;             // name `i` refers to object `[1, 2, 3]`
j = i;                   // name `j` refers to referent of `i`
i->elements[0] = &five;  // leading element of `i` refers to object `5`
```

<iframe width="800" height="500" frameborder="0" src="https://pythontutor.com/iframe-embed.html#code=import%20copy%0Adef%20fib%28n%29%3A%0A%20%20%20%20if%20n%20%3D%3D%200%3A%0A%20%20%20%20%20%20%20%20return%201%0A%20%20%20%20if%20n%20%3D%3D%201%3A%0A%20%20%20%20%20%20%20%20return%201%0A%20%20%20%20else%3A%0A%20%20%20%20%20%20%20%20return%20fib%28n-2%29%2Bfib%28n-1%29%0Adef%20dec%28g%29%3A%0A%20%20%20%20cnt%20%3D%200%0A%20%20%20%20def%20wrapper%28*argv%29%3A%0A%20%20%20%20%20%20%20%20nonlocal%20cnt%0A%20%20%20%20%20%20%20%20cnt%20%3D%20cnt%20%2B%201%0A%20%20%20%20%20%20%20%20print%28argv,cnt*%22-%22,%20'Decorated!'%29%0A%20%20%20%20%20%20%20%20ans%20%3D%20g%28*argv%29%0A%20%20%20%20%20%20%20%20cnt%20%3D%20cnt%20-%201%0A%20%20%20%20%20%20%20%20print%28cnt*%22-%22,'rev'%29%0A%20%20%20%20%20%20%20%20return%28ans%29%0A%20%20%20%20%0A%20%20%20%20return%28wrapper%29%0A%20%20%20%20%0Adef%20cov%28f%29%3A%0A%20%20%20%20e%20%3D%20copy.deepcopy%28f%29%0A%20%20%20%20e%20%3D%20dec%28e%29%0A%20%20%20%20return%20e%0At%20%3D%20cov%28fib%29%0Afib%20%3D%20lambda%20x%3A%20x%20%2B%202%0At%284%29&codeDivHeight=400&codeDivWidth=350&cumulative=false&curInstr=20&heapPrimitives=nevernest&origin=opt-frontend.js&py=3&rawInputLstJSON=%5B%5D&textReferences=false"> </iframe>


```py
import copy
def fib(n):
    if n == 0:
        return 1
    if n == 1:
        return 1
    else:
        return fib(n-2)+fib(n-1)
def dec(g):
    cnt = 0
    def wrapper(*argv):
        nonlocal cnt
        cnt = cnt + 1
        print(argv,cnt*"-", 'Decorated!')
        ans = g(*argv)
        cnt = cnt - 1
        print(cnt*"-",'rev')
        return(ans)
    
    return(wrapper)
    
def cov(f):
    e = copy.deepcopy(f)
    e = dec(e)
    return e
fib = cov(fib) really means
g = cov(fib)
fib = g

t(4)
```

Scoping + pyObject pointerlike behavior means we can post modify global scoped functions after wrapping them.

It is possible to design a hacking solution for a wrapper. Why must we do assignment in the global scope `f=dec(f)`? Because the globalscope is the scope that allows us full access to all the variable names aka pointers to our target function which is what we need access to, to redirect them to our wrapped function.

```py
def fib(n):
    if n == 0:
        return 1
    if n == 1:
        return 1
    else:
        return fib(n-2)+fib(n-1)
def big(f):
    print(f.__name__)
    
    def dec(g):
        cnt = 0
        def wrapper(*argv):
            nonlocal cnt
            cnt = cnt + 1
            print(argv,cnt*"-", 'Decorated!')
            ans = g(*argv)
            cnt = cnt - 1
            print(cnt*"-",'rev')
            return(ans)

        return(wrapper)
    globals()[f.__name__]=dec(globals()[f.__name__])
    return 0

big(fib)
fib(5)
```