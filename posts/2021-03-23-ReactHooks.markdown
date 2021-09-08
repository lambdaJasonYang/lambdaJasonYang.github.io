---
title: React Hooks
tags: musings
---

UseRef

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