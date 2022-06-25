---
title: Promises Callback GraphQL and fetch
tags: prog, cloud, frontend
---

[callcc](https://pavpanchekha.com/esp/continuations.html)

# Callback and Promises

$$ (3^2) + (5 \times 6)$$

* We compute $(5 \times 6)$ first, then pass the result
  * $\lambda k. k(30)$
    * $\text{k is } \lambda x. (3^2) + x$ 



```{.py group="typical" glabel="py"}
def plus(x, y, k):
    return k(x + y)
def mul(x, y, k):
    return k(x * y)
def square(x,k):
    return k(x^2)
mul(5,6,lambda x: (3**2) + x)

```

```{.md group="lambda" glabel="mul"}
    %k
     |
     @
    / \
   k  mul
      / \
     5   6 
```
```{.md group="lambda" glabel="sq"}
    %k
     |
     @
    / \
   k  sq
       |
       3 
```
```{.md group="lambda" glabel="plus"}
    %k
     |
     @
    / \
   k  plus
      / \
     x   y
```


# Graphql

```{.js group="graphql" glabel="promise"}
fetch("https://graphqlzero.almansi.me/api", {
  "method": "POST",
  "headers": { "content-type": "application/json" },
  "body": JSON.stringify({
    query: `{
      user(id: 1) {
        id
        name
      }
    }`
  })
}).then(res => res.json())
  .then(jsonoutput => console.log(jsonoutput))
// { "data": { "user": { ... } } }
```
```{.js group="graphql" glabel="promise"}
const res = async () => {fetch("https://graphqlzero.almansi.me/api", {
  "method": "POST",
  "headers": { "content-type": "application/json" },
  "body": JSON.stringify({
    query: `{
      user(id: 1) {
        id
        name
      }
    }`
  })
})}
const jsonoutput = async ()=>{res.json()}
console.log(jsonoutput)
// { "data": { "user": { ... } } }
```

