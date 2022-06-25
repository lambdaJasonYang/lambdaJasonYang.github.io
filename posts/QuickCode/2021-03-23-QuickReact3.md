---
title: Quick React Part 3 - Rerenders and VanillaJS
tags: prog, QuickCode, cloud, frontend
---

**On rerender the vanilla JS `let` variables and eventlisteners in the localenv are "reset"**   
**Basically everything gets reinitialized except the `useState` `useRef` variables**  

* Solution is to wrap the canvas and toggle variable in useRef
  * below `toggle.current` is unaffected by rerenders
  * allow the event listener to be rebuilt each time 

```js
const Junk = () => {
    let toggle = useRef(true); //GOOD
    //let toggle = true; //BAD, resets every render
    const width = 300;
    const height = 300;
    const [nully,forceRender] = useReducer(x=>x+1,0);
    useEffect(()=>{
        const toggleDraw = (e) => {
            if(toggle.current == true){
                let mX = e.clientX - width;
                let mY = e.clientY - height;
                console.log(tempX,tempY);
            }
        }
        const toggleCallBack = (e) => {
            if(e.code == "KeyG"){
                toggle.current = !toggle.current;
            }
        }
        document.addEventListener("mousemove",toggleDraw,false);
        document.addEventListener("keydown",toggleCallBack,false);
        return(()=>{
            console.log("unlinked") //called every rerender
            document.removeEventListener("mousemove",toggleDraw,false);
            document.removeEventListener("keydown",toggleCallBack,false);
        })
    });
    return(
        <div>
            <canvas id="tutorial" width={width} height={height} style={{border: "1px solid black"}} />
            <input type="button" value="hi" onClick={(e)=>{console.log(toggle)}}/>
            <input type="button" value="force" onClick={(e)=>{forceRender()}}/>
        </div>
    )
}
```


# Wrappers and prop.children

* Use `prop.children` if the component is used to wrap over other components .
* Warning JSX uses `"onMouseMove"` but vanillajs uses `"mousemove"`

```js
const Mouse = (props) => ..
  return
    <div onMouseMove={(e)=>{console.log(e.clientX)}}>
      {props.children}
    </div>
  

export Page = () => ..
    return
        <Mouse>
            <article>...
            <body>...
        </Mouse>
```

