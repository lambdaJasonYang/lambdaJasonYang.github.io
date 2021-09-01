---
title: Programming Lang Idiosyncrasies and Similarities
tags: prog
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