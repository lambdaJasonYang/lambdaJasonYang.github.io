---
title: Typescript
tags: prog, cloud, frontend
toc: y
---




mkdir someproj
npm init
npm install --save-dev typescript

modify package.json to include 
"scripts": {... "tsc" : "tsc"}

create tsconfig.json


* module
  * es2022 - `import { something } as "somepath"
  * commonjs - `require('something')`
* target - affects output of js code
  * es2022 - newest features but less compatible
* moduleResolution - how your project searches for module files in folder
  * node - use this
  * classic
* lib - 
  * "es2022","dom"
    * "dom" gives us access to browser-based globals like `window` or `document`