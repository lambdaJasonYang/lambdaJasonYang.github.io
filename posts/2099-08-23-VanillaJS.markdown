---
title: VanillaJS mini scripts
tags: prog
---
Some random JS scripts that I come back to frequently for webscraping or cross site scripting practice.

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


