---
title: Everything is a pointer perspective
tags: OOP, prog
toc: y
---

> Everything in python is a PyObject

Assignment in python are pointers binding.

* Reassignment of whole objects, DOES NOT mutate current objects; It CREATES new literal objects then binds.  
`a = 4`  basically the same as `a = new num(4)`  
* Reassignment of **properties** like list elements or object properties DOES mutate current objects.  
`a.age = 4`  

# Reassignment to whole objects

```{.py group=k1 glabel=simp}
a = 1 # a points to num(1)
b = a # b points to The object a points to, num(1)
a = 2 # a points to a new object num(2)
print(b)
```

```{.py group=k1 glabel=class}
class num():
    def __init__(self,x):
        self.x = x
a = num(1)
b = a
print(b.x) #1
a = num(2)
print(b.x) #1
```
`a` **DOES NOT CAUSE OBJECT MUTATION FROM num(1) to num(2)**    
Instead we just pointed to a different object.   


```Cpp
int* a = new int(1);
int* b = a;
a = new int(2);
cout << *b << endl;   // prints 1
```

# Reassignment of object properties

```py
# notice this doesnt cause mutation since we assign WHOLE objects
class num():
    def __init__(self,x):
        self.x = x
a = num(1)
b = a
print(b.x) #1
a = num(2) #ASSIGN WHOLE OBJECTS
print(b.x) #1

# but notice this does cause mutation since we assign PROPERTIES of object
class num():
    def __init__(self,x):
        self.x = x
a = num(1)
b = a
print(b.x) #1
a.x = 2    #ASSIGN PROPERTIES OF OBJECT
print(b.x) #2
```

```Cpp
int* a = new num(6);
int* b = a;
cout << *b << endl; //6
a->x = 7;
cout << *b << endl;   //7
```
