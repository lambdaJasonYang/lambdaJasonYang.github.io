---
title: Dom Tree rendering in VanillaJS and React
tags: prog, cloud, frontend
toc: y
---

1. Dom Tree rendering 
    * Dom tree can be blocked(partial parsing) from rendering
    * images are not loaded or have 0 height and width
    * CSS not loaded
    * JS not loaded
2. CSSOM(CSS Object model) aka CSS Tree rendering
    * CSS can block JS rendering
3. JS script tags will block the DOM tree from rendering
    * JS that modifies DOM elements must be placed in the footer
    * Async can also prevent JS from blocking DOM tree parsing
4. Render tree = DOM + CSSOM
5. Layout, Render tree will check the `<meta viewport=...>` tag and device screen to determine pixel size
6. Painting

Observer the rendering process in chrome inspect

* Chrome Inspect > Performance > Reload and Record
  * Select Event Log, to observe the rendering process
<!--  -->

# Loading libraries and scripts

* The code below wait until the DOM and JS libraries are loaded

```js
<script async src="somelibrary.js"/>  

<script async>
window.addEventListener('load', () => {
    plot.init(document.querySelector("canvas"))
})
</script>
```

* `<script defer>` ONLY WAITS FOR DOM PARSED


# Easy mistakes

## inline CSS + JS plots libraries

* JS libs render plots by having js modify a DOM element
  * inline CSS styles CREATES ERRORS because inline style may not load yet but js will still modify the DOM element resulting in a 0 height 0 width element.  
  * **Do not use inline CSS style tags**
  * Solution: put the CSS in the CSS file!  
  

# React

```js
window.addEventListener("load",async ()=>{
    const data = await fetch("https://...")  //1. fetch data
    const builtElement = build("button",data) //2. build JSX-like component
    document.querySelector("div").appendChild(builtElement) //3. Modify and render DOM tree
```

| VanillaJS | React |
| --- | --- | 
| JSX-like DOM component pre-filled w/ data | Separation btw JSX component and data fetch |
| 1. fetch data | fetch w/ `useEffect()` |
| 2. build 3. Modify, render tree | Build+render w/ `return(<><BlehComponent/></>)` |
| Sequential 1. fetch 2. build 3. Modify render tree | (fetch data) independent wrt (build+render tree)  |

Since we **cannot sequentially order data fetching in react with component building and rendering**,  
we MUST **prepare react JSX DOM components to accept empty data** on first render otherwise we will get errors

 
## React Conditional Rendering:

* `{someState && <Component someState={someState}>}` use this pattern when you want component to show only when it is filled with data
* `{<placeholderComponent/> || <Component someState={someState}>}` use this pattern for a placeholder image or element

```js
const BlehComponent = ({age}) => {...}
const SomeComponent = () => {
    const [age,setage] = useState();
        
    useEffect(()=>{
        //this is only called AFTER the first render
        fetch("https://data").then(response => setage((x)=> response.text())) 
    },[])
        
        //building and rendering JSX component
        //is not sequential with data fetching
    return(<div>
        {age && <BlehComponent age={age}>}
     </div>)
}
```