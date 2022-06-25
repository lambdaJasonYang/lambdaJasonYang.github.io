---
title: Quick React Part 2
tags: prog, QuickCode, cloud, frontend
---

# Intro

Component goes through 3 phases. 
* Function Body
* useEffect
* return()

In each phase code is ran by the order they are declared. (eg. if we have 2 useEffect() on line 2 and line 5, the first one at line 2 will always run first)

```js
const App = () => {
  // function body START
  const a = 3;
  const b = new TextEncoder()
  // function body END
  
  // useEffect START
  useEffect(()=>{
      const c = 6;
  },[])
  // useEffect END

  //return() START
  return(<div>my stuff</div>)

  //return() END
}
```

# Composition/Extending Components

* `FancyBorder :: (color,h1-className,pclassName, title, message) -> FancyBorderComponent`
* `Dialog = FancyBorder ("blue","Dialog-title","Dialog-message") :: (title,message) -> FancyBorderComponent`
* `WelcomeDialog = Dialog ("Welcome","Thank you for visiting our spacecraft!") :: FancyBorderComponent`

```js
function Dialog(props) {
  return (
    <FancyBorder color="blue">
      <h1 className="Dialog-title">
        {props.title}
      </h1>
      <p className="Dialog-message">
        {props.message}
      </p>
    </FancyBorder>
  );
}

function WelcomeDialog() {
  return (
    <Dialog
      title="Welcome"
      message="Thank you for visiting our spacecraft!" />
  );
}

```

# SSR and window object

PROBLEM: The below will fail because the page is rendered on the server, meaning there is no `window` object.

```{.js filename="error.js"}
const bleh:NextPage = () => {
  return(
    <div>{window.clientInformation}</div>
  )

}
```

SOLUTION: Use-effect is clientside render

## Convert JS Api objects to React usable hooks

[mozilla web api](https://developer.mozilla.org/en-US/docs/Web/API) has a list of API's for vanilla JS. Typically you call a global object then use the object's function to use the API.  

Ideally in functional React, we don't like to call global variables so we use hooks.  

```{.js filename=bad.js}
const bleh:NextPage = () => {
    const bleh = new TextEncoder() //BAD
    useEffect(()=>{
    },[])
    return(<div>
        {bleh && console.log(bleh.encode("a"))}
        </div>)
}
```
the `new` is a side-effect. Remember to only put side-effects in hooks.

```{.js filename=good.js}
const bleh:NextPage = () => {
    const bleh = new TextEncoder() //BAD
    useEffect(()=>{
    },[])
    return(<div>
        {bleh && console.log(bleh.encode("a"))}
        </div>)
}
```
# Closures

```hs
-- untyped lambda calculus values are functions
data Value = FunVal (Value -> Value)

-- we write expressions where variables take string-based names, but we'll
-- also just assume that nobody ever shadows names to avoid having to do
-- capture-avoiding substitutions

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Abs Name Expr

-- We model the environment as function from strings to values, 
-- notably ignoring any kind of smooth lookup failures
type Env = Name -> Value

-- The empty environment
env0 :: Env
env0 _ = error "Nope!"

-- Augmenting the environment with a value, "closing over" it!
addEnv :: Name -> Value -> Env -> Env
addEnv nm v e nm' | nm' == nm = v
                  | otherwise = e nm

-- And finally the interpreter itself
interp :: Env -> Expr -> Value
interp e (Var name) = e name          -- variable lookup in the env
interp e (App ef ex) =
  let FunVal f = interp e ef
      x        = interp e ex
  in f x                              -- application to lambda terms
interp e (Abs name expr) =
  -- augmentation of a local (lexical) environment
  FunVal (\value -> interp (addEnv name value e) expr)
```

# Hooks

https://www.netlify.com/blog/2019/03/11/deep-dive-how-do-react-hooks-really-work/

```js
// Example 0
function useState(initialValue) {
  var _val = initialValue // _val is a local variable created by useState
  function state() {
    // state is an inner function, a closure
    return _val // state() uses _val, declared by parent funciton
  }
  function setState(newVal) {
    // same
    _val = newVal // setting _val without exposing _val
  }
  return [state, setState] // exposing functions for external use
}
var [foo, setFoo] = useState(0) // using array destructuring
console.log(foo()) // logs 0 - the initialValue we gave
setFoo(1) // sets _val inside useState's scope
console.log(foo()) // logs 1 - new initialValue, despite exact same call
```

What this says is _val 