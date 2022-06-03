---
title: Graph libraries in JS
tags: prog, QuickCode, cloud, frontend
---

sigmajs+graphology+grapology-lib

# SigmaJS

```html
<div id="sigma-container" "></div>

div#sigma-container {
  height: 50vh;
  width: 70vw;
}
```

```js
window.addEventListener('load', () => {
  const container = document.getElementById("sigma-container");
  const graph = new graphology.Graph();
  graph.addNode("Home", { x: 0, y: 0, size: 5, label: "Home", color: "blue",URL : "/#" });
  graph.addNode("MathCS", { x: 1, y: 1, size: 5, label: "MathCS", color: "red" , URL: "/tags/mathcs.html"});
  graph.addNode("Programming", { x: 1, y: -1, size: 5, label: "Prog", color: "red", URL: "/tags/prog.html" });
  graph.addNode("QuickCode", { x: 2, y: 0, size: 5, label: "Prog", color: "red", URL: "/tags/ML.html" });
  graph.addNode("functional", { x: 2, y: 1, size: 5, label: "Prog", color: "red", URL: "/tags/ML.html" });

  graph.addEdge("Home", "MathCS");
  graph.addEdge("Home","Programming");
  graph.addEdge("Programming","QuickCode");
  const settingsSigma = {
    labelRenderedSizeThreshold: 1,
  }
  const renderer = new Sigma(graph,container,settingsSigma);
  renderer.on("clickNode",(e)=>{
    const nodeName = e.node
    const urltag = graph._nodes.get(nodeName).attributes.URL
    window.location.assign(window.location.origin+urltag)
    console.log(window.location.origin + urltag); 
    
  });
  
})
```

# Cytoscape



