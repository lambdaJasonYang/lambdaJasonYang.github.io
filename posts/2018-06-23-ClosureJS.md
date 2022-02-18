---
title: Closures in JS
tags: prog, cloud, frontend
---

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