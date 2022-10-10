---
title: JS Async Await
tags: prog, cloud, frontend
toc: y
---



Await <--> Functions that return Promises  
Async <--> Functions that **CONTAINS** Await functions  

```.txt
Await Functions <---> Promise-returning Functions
    ^                       ^
    |                       |
   contains               contains
    |                       |
Outer Functions <---> Async Functions
```

**NOTE: `await` can be used on Regular non-Promise returning Functions but it would do nothing.**

* We can add `await` to non-promise returning function and wrap it around an `async` function BUT its stupid and pointless.
  * example below:
    * `async function AsyncRegularFunction(msg,wtime){..` is the same as `function RegularFunction`
    * `await RegularFunction("noPromiseSleep",1000)` call is same as `RegularFunction("noPromiseSleep",1000)` call

```js
function PromiseFunction(msg,wtime) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve(msg);
    }, wtime);
  });
}

//No-promise Regular Function
function RegularFunction(msg,wtime){
 	setTimeout(() => {
      console.log(msg)
      return msg
    }, wtime);
}

async function AsyncPromiseFunction(msg,wtime){
  	console.log("inside AsyncPromiseFunction call")
 	return PromiseFunction(msg,wtime);
}

//AsyncRegularFunction is the same as RegularFunction
//Therefore the function below is a stupid function and should never be called or made
async function AsyncRegularFunction(msg,wtime){
  	console.log("inside AsyncRegularFunction call")
 	return RegularFunction(msg,wtime); 
}

async function mainCall() {
  console.log("start");
  //`await RegularFunction` is same as `RegularFunction`
  await RegularFunction("RegularFunction_noPromise",1000);
  RegularFunction("RegularFunction_noPromise",1000);
  console.log("done");
  // expected output: "resolved"
}

mainCall();


```

# Normal function behavior

* The theme is that **typical functions(non-promise) in JS is not really predictable or truly sequential even though it may seem that way!!**

```js
function mainCall() {
  console.log("start");
  RegularFunction("RegularFunction_noPromise",2000);
  console.log("done");
  // expected output: "resolved"
}

> "start"
> "done"
delay 2s
> "RegularFunction_noPromise"
```

* Watch that rather than sequentially blocking for the RegularFunction, it skips it and and runs `console.log("done");`.


# With promise

## always put await next to promise function call

* Do we put `await` outside of `console.log` or inside?
  * inside, right next to the function that returns a promise

```{.js group=1 glabel="correct"}
async function mainCall() {
  console.log("start");
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",1000));
  console.log("done");
  
}

> "start"
> "inside AsyncPromiseFunction call" 
delay 2s
> "AsyncPromiseFunction"
> "done"
```


```{.js group=1 glabel="wrong"}
async function mainCall() {
  console.log("start");
  await console.log(AsyncPromiseFunction("AsyncPromiseFunction",1000));
  console.log("done");
  
}

> "start"
> "inside AsyncPromiseFunction call" 
> [object Promise]
> "done"
```

## Await blocks Everything that comes after

* `RegularFunction` comes BEFORE the `console.log(await AsyncPromiseFunction("AsyncPromiseFunction",2000));` meaning the `RegularFunction` does not get blocked.    

Here are 2 different timings, but in both cases, `RegularFunction` call and `AsyncPromiseFunction` call runs as if they were independent. 

```{.js group=1A glabel=timing1}
async function mainCall() {
  console.log("start");
  RegularFunction("RegularFunction_noPromise",3000); //BEFORE the await does NOT get blocked
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",2000));//AFTER the await gets blocked
  console.log("done");//AFTER the await gets blocked
  
}

> "start"
> "inside AsyncPromiseFunction call"
delay 2
> "AsyncPromiseFunction"
> "done"
delay ~0.5 to 1
> "RegularFunction_noPromise"
```


```{.js group=1A glabel=timing2}
async function mainCall() {
  console.log("start");
  RegularFunction("RegularFunction_noPromise",2000); //BEFORE the await does NOT get blocked
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",3000));//AFTER the await gets blocked
  console.log("done");//AFTER the await gets blocked
  
}

> "start"
> "inside AsyncPromiseFunction call"
delay 2
> "RegularFunction_noPromise"
delay ~0.5 to 1
> "AsyncPromiseFunction"
> "done"
```

When you mix regular function calls with properly made `await` function calls, they behave as if running in parallel other wise the `delay ~0.5 to 1` should be `delay 3`



* Here is the scenario where `RegularFunction` comes AFTER the `console.log(await AsyncPromiseFunction("AsyncPromiseFunction",2000));` meaning the `RegularFunction` does get blocked. 

```js
async function mainCall() {
  console.log("start");
  
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",2000));
  RegularFunction("RegularFunction_noPromise",4000); //AFTER the await gets blocked
  console.log("done"); //AFTER the await gets blocked
  
}

> "start"
> "inside AsyncPromiseFunction call"
delay 2
> "AsyncPromiseFunction"
> "done"
delay 4
> "RegularFunction_noPromise"
```

* Notice how the `await AsyncPromiseFunction` blocks **ALL** the code underneath it.
  * By this train of thought, to make code SORTA **sequential**, just use `await` on all the promise-function calls.
    * We say SORTA because notice in the code above, the result of `RegularFunction("RegularFunction_noPromise",4000);` and `console.log("done");` switched places meaning **we can't control behavior of non-promise regular functions.**(The motto of javascript)

# Sequential await

```js
async function mainCall() {
  console.log("start");
  
  console.log(await AsyncPromiseFunction("AsyncPromiseFunctionA",2000));
  console.log(await AsyncPromiseFunction("AsyncPromiseFunctionB",1000));
  console.log(await AsyncPromiseFunction("AsyncPromiseFunctionC",3000));
  console.log(await AsyncPromiseFunction("AsyncPromiseFunctionD",2000));
  
  console.log("done");
}

> "start"
> "inside AsyncPromiseFunction call"
> "AsyncPromiseFunctionA"
> "inside AsyncPromiseFunction call"
> "AsyncPromiseFunctionB"
> "inside AsyncPromiseFunction call"
> "AsyncPromiseFunctionC"
> "inside AsyncPromiseFunction call"
> "AsyncPromiseFunctionD"
> "done"

```

# Parallel

```js
async function mainCall() {
  console.log("start");
  console.time('myClock')
  parallel = await Promise.all([
     AsyncPromiseFunction("AsyncPromiseFunctionA",6000),
     AsyncPromiseFunction("AsyncPromiseFunctionB",1000),
     AsyncPromiseFunction("AsyncPromiseFunctionC",3000),
     AsyncPromiseFunction("AsyncPromiseFunctionD",2000),])
  console.log(parallel);
  console.timeEnd('myClock')
  console.log("done");
  
}

> "start"
> "inside AsyncPromiseFunction call"
> "inside AsyncPromiseFunction call"
> "inside AsyncPromiseFunction call"
> "inside AsyncPromiseFunction call"
> ["AsyncPromiseFunctionA","AsyncPromiseFunctionB","AsyncPromiseFunctionC","AsyncPromiseFunctionD"]
> "myClock: 6001.09375 ms"
```

When ran in parallel, the execution time is only as long as the longest function which is `AsyncPromiseFunction("AsyncPromiseFunctionA",6000)`




# Showcase stupid usage of async await with typical non-promise functions

would `await AsyncRegularFunction("AsyncNoPromiseSleep",2000);` or `await RegularFunction("AsyncNoPromiseSleep",2000);` or `AsyncRegularFunction("AsyncNoPromiseSleep",2000);` do anything different? NO!!

```{.js group=2 glabel=GOOD}
async function mainCall() {
  console.log("start");
  RegularFunction("RegularFunction",2000);
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",3000));
  console.log("done");
  
}

> "start"
> "inside AsyncPromiseFunction call"
delay 2
> "RegularFunction"
delay ~0.5 to 1
> "AsyncPromiseFunction"
> "done"
```

```{.js group=2 glabel=STUPID_A}
async function mainCall() {
  console.log("start");
  await RegularFunction("RegularFunction",2000);
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",3000));
  console.log("done");
  
}

> "start"
> "inside AsyncPromiseFunction call"
delay 2
> "RegularFunction"
delay ~0.5 to 1
> "AsyncPromiseFunction"
> "done"
```

```{.js group=2 glabel=STUPID_B}
async function mainCall() {
  console.log("start");
  AsyncRegularFunction("AsyncRegularFunction",2000);
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",3000));
  console.log("done");
  
}

> "start"
> "inside AsyncRegularFunction call"
> "inside AsyncPromiseFunction call"
delay 2
> "AsyncRegularFunction"
delay ~0.5 to 1
> "AsyncPromiseFunction"
> "done"
```

```{.js group=2 glabel=STUPID_C}
async function mainCall() {
  console.log("start");
  await AsyncRegularFunction("AsyncRegularFunction",2000);
  console.log(await AsyncPromiseFunction("AsyncPromiseFunction",3000));
  console.log("done");
  
}

> "start"
> "inside AsyncRegularFunction call"
> "inside AsyncPromiseFunction call"
delay 2
> "AsyncRegularFunction"
delay ~0.5 to 1
> "AsyncPromiseFunction"
> "done"
```

The code above all return the same results and are functionally correct but the stupidity will confuse other developers.

**REMEMBER TO STOP USING await with Non-promise functions**