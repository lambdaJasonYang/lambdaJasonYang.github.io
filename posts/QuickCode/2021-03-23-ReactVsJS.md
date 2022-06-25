---
title: React vs JS
tags: prog, QuickCode, cloud, frontend
---

Build a textbox that reactively logs the current value of the textbox as you input

```js
   <input style="margin:10" type="text" maxLength=3 name="box1"/>
    <input style="margin:10" type="text" maxLength=3 name="box2"/>
  <script>
    let box1 = document.querySelector("input[name=box1]");
    let box2 = document.querySelector("input[name=box2]");
    box1.addEventListener("input",(e) => {
      console.log(e.target.value);
    });
  </script>
```

```js
const [getVal_box1,setVal_box1] = useState("")

<Input 
onChange={(e)=>{
  setVal_box1((old)=>e.target.value); 
  console.log(getVal_box1());
}}
/>
```
