---
title: Conditional rendering JS React
tags: prog,js, frontend
toc: y
---


```js
(undefined || []) --> []
```
# JS

## Map Filter Reduce 

undefined array will crash so fallback to empty array.

```js
SomeArr = mkArr(..) //undefined
(SomeArry || []).filter(...)
```

# JSX

Render only when dataSet is defined

```js
<div>
dataSet && <Chart info={dataSet}/>
</div>
```
