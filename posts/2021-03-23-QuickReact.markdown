---
title: Quick React
tags: prog, QuickCode, cloud, frontend
---

* React components have 2 parts 
  * localenv: local environment's variable,hooks,functions 
  * JSX: the JSX or virtual DOM it returns.   

The moment we enter a new state, 0.1 and 0.2 is what happens first.

***localenv behaves asynchronously*** Don't trust your own ordering of ESPECIALLY WHEN INITAIZLING JSON variables because your INIT may depend on side effects it may mix side effects.
```javascript
App = ({data}) => {
//data outputs some modified garbage data 
//because of asynchronous side effect below
console.log(data) 
/////////////////////////////////////////
const optionVar = {
  price: SideEffectFunc(data) 
  //BIG NONO
}
/////////////////////////////////////////
return(..JSX..)
}
//1st fetches data from db
export async func getStaticProps(context){
  const data = fetch(...)
  return data
}
```

Initializing a json "optionVar" but initialization depends on a sideeffect function.
We expect data to output clean data but the "SideEffectFunc" already touched and corrupted our data.
**Server side on our console it looks like clean data BUT WHEN RENDERED USING chrome inspect we see data is actually modified**

* DO NOT USE STATEFUL VARIABLES OR SIDE EFFECTS IN localenv
* DO NOT MODIFY THE "props" IN localenv



```plantuml
@startuml
skinparam shadowing false
state 0 as "state 0" {
  state "localenv" as lenv1 <<sdlreceive>>
  state "jsx" as jsx1 <<sdlreceive>>
  lenv1 -[#blue]-> lenv1 : 0.1. Render 
  jsx1 -[#blue]-> jsx1 : 0.2. Render
  [*] --> jsx1 : 1. click button
  jsx1 --> lenv1 : 2. callback calls upgradehp(){... sethp(.)}
  lenv1 --> lenv1 : 3. upgradehp() runs to \ncompletion with no state change midway
  
  
}

state 1 as "state 1" {
  state "localenv" as lenv2 <<sdlreceive>>
  state "jsx" as jsx2 <<sdlreceive>>
  lenv2 -[#blue]-> lenv2 : 0.1 Render
  jsx2 -[#blue]-> jsx2 : 0.2 Render
  [*] -[dotted]-> jsx2 : 5. See updated page
}
lenv1 -[#green]-> 1 : 4. state change
@enduml
```


```{.javascript .numberLines}
const App = () => {
//LOCALENV START ------------
console.log("0.1 render")
const hp, sethp = useState(() => 0)
const upgradehp() = {
    console.log(hp) //2. outputs 0
    sethp(prevhp => prevhp*2)
    console.log(hp) //2. outputs 0
}//3. after running to completion our state update takes effect
//LOCALENV End --------------

//JSX START ------------------
return (
    <div>
    //1. click some button that calls upgradehp()
    {console.log("0.2 render")}
    {hp}
    </div>
)
//JSX End --------------------
}
```

Line 6 and Line 8 will always output the same value.
The local env maintains some level of functional purity.  

The pure upgradehp() function will run through completion (ignoring the state change of hp)  
but after finishing it will signal to the JSX that hp state has changed and rerender the component to a new state.

In many ways the upgradehp() is similar to being an IO monad.

To understand react, trust that every function behaves in a functional deterministic manner wrt to it's current state. (shown in line 6 and line 8)



### When does React component rerender

* setState hook() is called
* All children of the component will also be rerendered



### UseState

```javascript
const hp, sethp = useState(() => 0)
const upgradehp() = {
    sethp(prevhp => prevhp*2)
    sethp(prevhp => prevhp*2)
}
...
```
* DO pass a lambda function `prevhp => prevhp*2`{.javascript} to setState 
* DO NOT do `sethealth(hp*2)`{.javascript} 
  * The behavior may be indeterminant if we called it twice like in the above code.

* we pass in a lazy function `() => 0`{.javascript} on useState so that it isn't called on every render.
  * Similar reason we pass a lazy callback function in jsx element like `onClick={(e) => doSomething()}`{.javascript}


### UseEffect

```javascript
useEffect(() => {
    //Mount begin--------------
    //setup event listener, subscribe api
    //mount
    console.log('target changed') 
    //Mount end----------------

    //UnMount begin------------
    return () => {
        console.log('return from unmount changed') 
        //clean up event listener, unsubscribe api
        //unmount
    }
    //UnMount end ------------
},
[] //target is empty
)
```

* Mount - can only happen once
* Update - can happen as much as possible
* unMount - can only happen once

**When UseEffect tracks a target or variable**  

* It will also call on mount/unmount but this time it will also call when the target is changed. 

```javascript
useEffect(() => {
    // begin--------------
    //on mount 1 call
    //setup event listener, subscribe api
    
    console.log('target changed') 
    // end----------------

    // begin------------
    return () => {
        console.log('return from unmount changed') 
        //clean up event listener, unsubscribe api
        //unmount
    }
    // end ------------
},
[target] 
)
```

### UseRef

```plantuml
@startuml
object asd
note left
 <&wifi>
end note
diamond s

@enduml
```

```{.javascript .numberLines}
const App = () => {
    const refContainer = useRef(null);
    const onButtonClick = () => {console.log(refContainer.current)}
    //refContainer.current will return the DOM object
    return(
        <>
            <div ref={refContainer}>SomeText </div>
            <button onClick={onButtonClick}>button</button>
        </>
    )
}
```
line 7, the JSX `<div ref={refContainer}>`{.javascript} is what binds the ref Hook to the Dom Element.

In most cases of hooks like useEffect or useState, it's the JSX element that depends on the hook.  
But in this case the hook depends on the JSX element.