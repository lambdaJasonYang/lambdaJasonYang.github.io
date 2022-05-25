---
title: Quick React 4
tags: prog, QuickCode, cloud, frontend
---

# UseMemo

```js
import "./styles.css";
import React, {useMemo,useState,useEffect} from 'react';
const slowfunction = (num) => {
  console.log("slow function");
  for(let i = 0 ; i <= 10000000; i++){};
  return num * 2;   
}

export default function App() {
  const [count,setCount] = useState(0);
  const [count2,setCount2] = useState(0);
  const memoizedslowfunction = useMemo(() => {
    return slowfunction(count)
  },[count]);

 
  return (
    
    <div className="App">
      
      <button onClick = {event => {
        setCount(cstate => cstate + 1)
        }}>Not Often pressed button</button>
        <p>Not Often Updated Value: {slowfunction(count)}</p>
        <p>Above count value needs to be memoized</p>
      <h2>----------</h2>


      <button onClick = {event => {
        setCount2(cstate => cstate + 1)
      }}>Often pressed button</button>
      <p>Often Updated Value: {count2}</p>
    </div>
  );
  }
```