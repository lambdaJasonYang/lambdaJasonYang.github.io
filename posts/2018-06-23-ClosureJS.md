---
title: Closures in JS, Python
tags: prog, cloud, frontend
---

* Closures are not related to , this, bind, or arrow functions.

# Closures = Class

* local env of function is the private scope
* the return content is the public scope

```js
var makeCounter = function() {
  //Private variable and functions START
  var privateCounter = 0;
  function privateChangeBy(val) {
    privateCounter += val;
  }
  //Private variable and functions END
  
  //Public variable and functions START
  return { 
    increment: function() {
      privateChangeBy(1);
    },

    decrement: function() {
      privateChangeBy(-1);
    },

    value: function() {
      return privateCounter;
    }
  }
  //Public variable and functions END
};

var newCounter = makeCounter() //similar to constructor
newCounter.increment() 
newCounter.value() //output 1
```

```python
class makeCounter{
    private privateCounter;
    private privateChangeBy(val){...};
    public increment();
    public decrement();
    public value();
}

```

# Closure in python vs JS

* closure in python requires use of `nonlocal` keyword

```py
def f():
    a=0
    def g():
        a+=1 #ERRor
        print(a)
    return g
g=f()
g()
```

```py
def f():
    a=0
    def g():
        nonlocal a
        a+=1 
        print(a)
    return g
g=f()
g()
```

```js
let f=()=>{
    let a=0
    let g=()=>{
        a+=1
        console.log(a)
        
    }
    return g
    
}
let g=f()
g()
```

