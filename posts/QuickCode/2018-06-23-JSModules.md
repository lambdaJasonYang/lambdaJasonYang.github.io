---
title: JS Modules and NPM packages
tags: prog, cloud, frontend
toc: y
---

# VanillaJS module

* We can import modules in vanilla JS like in `index.js` as long we give attribute `type="module"` 


```{.html filename=index.html}
<!DOCTYPE html>
<html>
  <head>
  </head>
  <body>
    <script type="module" src="index.js"></script>
  </body>
</html>
```

```{.js filename=index.js}
import { bleh } from "./mymodule.js"
document.body.textContent = bleh;
```

```{.js filename=mymodule.js}
export const bleh = "shit";
```


# Build your own NPM package module 

How do we make our own npm package and run it locally?  

## Node module

1. Make directory `producerLib` for our library  
2. Go to `producerLib` and run `npm init`  
The entry point in package.json is `index.js` therefore we MUST name our file `index.js`  

```{.js filename=/producerLib/index.js}
module.exports = (x) => {
    return `hi ${x}`
}
```

## Client consumer

1. Make directory `consumer`  
2. Go to `consumer` and run `npm init`  
Since this is NOT a package or library but an application we can name our file anything like `app.js`  
despite the package.json telling us the entry point is `index.js`

:::{group=1 glabel=requireJS}

```{.js filename=/consumer/app.js}
//import greeter from "producerLib"
const greeter = require("producerLib")

console.log(greeter("he"))
```
```{.json filename=/consumer/package.json}
{
  "name": "consumer",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "",
  "license": "ISC"
}
```
:::

:::{group=1 glabel=ESModule}

```{.js filename=/consumer/app.js}
import greeter from "producerLib"
// const greeter = require("producerLib")

console.log(greeter("he"))
```
```{.json filename=/consumer/package.json}
{
  "name": "consumer",
  "type": "module",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "",
  "license": "ISC"
}
```
:::

Notice that clients using ESmodule imports requires `package.json` to include `"type": "module"`  
which is not required for clients using requireJS imports.


* Go to directory producerLib and run `npm link`
* Go to directory consumer and run `npm link producerLib`  

Run consumer with `node app.js`  


# RequireJS vs ESModule syntax

* `const bleh = require("somelib")` vs `import bleh from "somelib`

Use ESModule; RequireJS is only there for backward compatibility