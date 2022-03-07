---
title: Quick React class
tags: prog, QuickCode, cloud, frontend
---

```js
import "./styles.css";
import React from "react";

class Hi extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      init: true
    };
    
    console.log("step 1");
  }
  forceRender(){
    this.setState({ init: false });
  }

  //useEffect(()=>{step1.2},[])
  componentDidMount() {
    console.log("step 1.2 called once on mount");
  }
  //useEffect(()=>{return(step1.15)},[])
  componentWillUnmount() {
    console.log("step 1.15? called on leave");
  }

  //useEffect(()=>{step2},[targetstate])
  componentDidUpdate() {
    console.log("step 2 called on force render but not on mount");
  }
  render() {
    return(<div>
      <button onClick={(e) => this.forceRender()}>Say something</button>
      hey{console.log("step 1.1")}</div>)
  }
}

export default function App() {
  return (
    <div className="App">
      <Hi />
      <h1>Hello CodeSandbox</h1>
      <h2>Start editing to see some magic happen!</h2>
    </div>
  );
}

```

* On first render (1 -> 1.1 -> 1.2)
* After clicking button (1.1 -> 1.2)
* After unmount aka closing (1 -> 1.1 -> 1.15? -> 1.2)

Notice in class component a constructor is called before render.  
In functional components, we do not have a constructor so we have to deal with rendering components with empty data.