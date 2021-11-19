---
title: VanillaJS mini scripts
tags: prog, cloud, frontend
---


##### Send HTML request with js
``` javascript
var xmlHttp = new XMLHttpRequest();
xmlHttp.onreadystatechange = function() {
    if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
        var response = JSON.parse(xmlHttp.responseText);
        document.querySelector("#content").innerHTML = response.value.joke;
    }
}
xmlHttp.open("GET", "https://api.icndb.com/jokes/random/", true);
xmlHttp.send(null);
```

##### Select element by class or id 


``` javascript
document.querySelector("#content").value = "new value";
//this will select by id

document.querySelector(".myclass");
//this will select by class
```
##### More Complex selector

``` javascript
var el = document.querySelector("div.user-panel.main input[name='login']");
```
``` HTML
<div class="user-panel main">
    <input name="login"/>
</div>

```


##### Building HTML element through vanilla JS
``` javascript
var paragraph = document.createElement("p");
var textContent = document.createTextNode("content");
paragraph.appendChild(textContent);

document.querySelector("#messages").appendChild(paragraph);
```

---

### Promises, Async, Timeout



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

### location.hash

Not the hash function! 
Given a url: `https://example.com/stuff#blah%20ads%20bah`{.js}


`location.hash`{.js} --> `#blah%20ads%20bah`{.js}

```js
decodeURIComponent(location.hash.substr(1))
```

---



### JS for React

* event object
  * React uses event as arguments to callbacks a lot

```javascript
const callback = (e) => {
    const domTarget = e.target
    console.log(e.type)
}
```

[List of event types](https://developer.mozilla.org/en-US/docs/Web/API/Event#interfaces_based_on_event) 
