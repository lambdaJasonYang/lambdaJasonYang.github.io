---
title: VanillaJS mini scripts
tags: prog, cloud, frontend
toc: y
---




# Send HTML request with js
``` javascript
let xmlHttp = new XMLHttpRequest();
xmlHttp.onreadystatechange = function() {
    if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
        let response = JSON.parse(xmlHttp.responseText);
        document.querySelector("div").innerHTML = response.value.joke;
    }
}
xmlHttp.open("GET", "https://api.icndb.com/jokes/random/", true);
xmlHttp.send(null);
```


```js
let responseText = await fetch("https://api.icndb.com/jokes/random/")
```

It's nearly impossible to set a promise respone to a variable but much easier with `await`

```js
let somePromise = fetch("https://api.icndb.com/jokes/random/")
// We still cant do anything with this
let anotherPromise = responseTextPromise.then(response => response.text())
// nope this just returns another promise
let anotheranotherPromise = anotherPromise.then(x => x)
```

Notice when we use promises, this is no way to send the response to set a variable,  
all we can do is infinitely chaining with more `.then(..)` which just returns yet another promise

# Select element by class or id 


``` javascript
document.querySelector("#content").value = "new value";
//this will select by id 

document.querySelector(".myclass");
//this will select by class 
```
# More Complex selector

``` javascript
var el = document.querySelector("div.user-panel.main input[name='login']");
```
``` HTML
<div class="user-panel main">
    <input name="login"/>
</div>

```


# Building HTML element through vanilla JS
``` javascript
var paragraph = document.createElement("p");
var textContent = document.createTextNode("content");
paragraph.appendChild(textContent);

document.querySelector("#messages").appendChild(paragraph);
```

---

# Promises, Async, Timeout



sdfs

---


```javascript
navigator.geolocation.getCurrentPosition((loc)=>{
    return `${loc.coords.latitude} and ${loc.coords.longitude}`
     })
```

```javascript
//Local storage data is perm
localStorage.setItem("key", "someVal");
localStorage.getItem("key");

//Session storage data gets deletd on browser close
sessionStorage.setItem("key", "someVal");
sessionStorage.getItem("key");
```

---

# location.hash

Not the hash function! 
Given a url: `https://example.com/stuff#blah%20ads%20bah`{.js}


`location.hash`{.js} --> `#blah%20ads%20bah`{.js}

```js
decodeURIComponent(location.hash.substr(1))
```

---



# JS for React

* event object
  * React uses event as arguments to callbacks a lot

```javascript
const callback = (e) => {
    const domTarget = e.target
    console.log(e.type)
}
```

[List of event types](https://developer.mozilla.org/en-US/docs/Web/API/Event#interfaces_based_on_event) 

# Web apis

* WebUSB Api - connect USB devices
* WebGL - render interactive 3d graphics
* Webassembly - native performance
* Bluetooth Api - Connect to wireless device
* Notification Api - Display system notification
* Service Worker - Caching for offline usage
* Web Workers - Multithread

# this and bind

* `this` is an OO construct that allows vision of the outer closure similar to `self` in python.
  * `this` varys depending on where it is instantiated, assigned or called 
  * On assignment to variable the `context` changes
    * `let a = b` means `context` of `b` depends on where this assignment happened.
  * `.bind()` allows us change the `context` 
<!--  -->
* idiosyncrasy:`const f = ()=>{}` keeps the `this` or `context` where the function was declared but `function(){}` does not.

```js
//global context
let dog = {
    //dog context
    sound : "wolf",
    talk : function(){ //note we didnt use arrow notation ()=>{}
        console.log(this.sound)
        //'this' -> dog context
    }
}
dog.talk() // prints wolf
let newdogtalk = dog.talk //we basically copied the talk function so 
//'this' now points to place of assignment which is global context
newdogtalk() //output undefined since there is no 'sound' in global


let newnewdogtalk = (dog.talk).bind(dog) //bind forces 'this' to point to dog context
newnewdogtalk() //output wolf
```

## Arrow vs function()

* Arrows binds this to the lexical scope, meaning you can just look at the code and see what encloses the arrow function to understand what `this` points to.
  * static binding
* function() binds dynamically, meaning `this` depends on how the function is called at run time.
  * dynamic binding

