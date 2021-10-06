---
title: Programming Lang Idiosyncrasies and Similarities
tags: prog, QuickLang
---


### Destructuring

Changing property "size" to 3:

* haskell record type
  *  `someRecordType{ size = 3}`{.haskell}
* js
  * `{...props,"size":3}`{.javascript}


### Case sensititivity

* Haskell functions must be lowercase
* React components must be Uppercase

### File naming

* Golang test files must end in "test"  
  * "helloworld_test.go"
* Golang test functions must begin with "Test"  
  * `func TestHello( *testing.T){...}`{.go}

### React

* React \<li key=.. \> must have a "key" property


### JS fetch 

Try
  fetch request with no-cors 
If returns opaque response, it means cors is not allowed by the website

const res = fetch(`https://bleh.com/examples/data/asset/data/tempdata.json`,{mode: "no-cors"}).then((data)=>{console.log(data.body)})

If data.body is a readable stream, you first have to convert it to 
Know that .json() returns a promise so you have to pass it again

const res = fetch(`https://bleh.com/examples/data/asset/data/tempdata.json`,{mode: "no-cors"}).then((re)=>{return re.json()}).then(response => {console.log(response)})