---
title: Quick Next
tags: prog, QuickCode
---

```bash
npx create-next-app --ts
npm install @emotion/react
npm install @emotion/eslint-plugin --save-dev
```

.eslintc.json
```json
{
  "env":{
    "browser": true,
    "es2021": true,
    "node":true
  },
  "parser":"@typescript-eslint/parser",
  "parserOptions":{
    "emcaVersion":2020,
    "sourceType":"module",
    "emcaFeatures":{
      "jsx":true
    },
    "project":"./tsconfig.json"
  },
  "extends": ["next/core-web-vitals","plugin:@typescript-eslint/recommended","prettier"],
  "settings":{
    "react":{
      "version":"detect"
    }
  }
}

```


nextjs routing 

Routing Depends on File/Folder organization

* .next
* package.json
* package-lock.json
* node_modules
* .git
* pages
  * [carID]
    * index.js
	* [modelID].js
  * [shipID]
    * index.js
	* [modelID].js
  * index.js
  
 nested route
  
 
https://mywebsite.com/[carID]/[modelID]
  
 
Capturing the [carID] and [modelID]
const router = useRouter()
const {carID, modelID} = router.query()
console.log(`the car is {carID} and model {modelID}`)

getStaticProps only runs serverside even though you may have written it on a clientside file like index.js
You can write serverside code in the getStaticProps function.

getStaticProps is only allowed in a page file, NOT in a component file.

getStaticProps only runs on build time, in dev mode we see it being rebuilt every request which may cause confusion.

Why SSG can be bad?
SSG at build - getStaticProps
SSG at first request - getStaticPaths = will not fetch at build but at first request and cache the page for every new request

SSG at build time and SSG at first request both are affected by stale data.
If db changes once, SSG at build time is already stale
If db changes more than once, SSG at first request becomes stale
This means if a single change happens in db we have to rebuild the ENTIRE site.

Solution ISR which basically means the specific static page ONLY will be rebuilt, and 
if your clientside code auto refreshes every 10 sec, the data will be relativly current. 


SSR