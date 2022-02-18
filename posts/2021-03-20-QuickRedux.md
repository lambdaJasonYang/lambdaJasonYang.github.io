---
title: Quick Redux
tags: prog, QuickCode, cloud, frontend
toc: y
---

* store: holds the global state
* dispatch: user event sends action object to reducer which reduces aka modifies the store
  * action: basically a json object
  * reducer: basically a switch statement that takes an action
<!--  -->

# store

```{.js filename=/app/store.tsx}
import { configureStore } from '@reduxjs/toolkit'
import counterReducer from '../app/counterSlice'

export const store = configureStore({
  reducer: {
    counter: counterReducer,
  },
})

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>
// Inferred type: {posts: PostsState, comments: CommentsState, users: UsersState}
export type AppDispatch = typeof store.dispatch
```



```{.js filename=SomeComponent.tsx}
const SomeComponent = () => {
    const count = useSelector((state: RootState) => state.counter.value)
    const dispatch = useDispatch()

    return(
        <div>
<input type="button" onClick={()=>dispatch(increment())} value="sdasd" />
            <div>{count}</div>
        </div>
    )
}
```
## READ TO STORE

* `useSelector` hook allows us to READ the things in the store
   * `useSelector` also behaves like useEffect: on each dispatch, it will check if it's output is diff and will rerender if it is.
     * example: if some random action gets dispatched `state.counter.value` will be observed to see if value changes

## WRITE TO STORE

* `useDispatch` hook gives us a function `dispatch(somAction)` 
  * What do actions look like?  `{ type: 'todos/todoAdded', payload: trimmedText }` . They are just objects with a `type:...` field

###  createSlice

* How do we build/define actions? using `createSlice({name: ...})
  * Example code defines 2 actions using a createSlice
    * `name: "Counter"` with `increment` defines action `{type: Counter/increment}`
    * `name: "Counter"` with `decrement` defines action `{type: Counter/decrement}`

```{.js filename=counterSlice.tsx}
import { createSlice, PayloadAction } from '@reduxjs/toolkit'

export interface CounterState{
    value: number
}

const initialState : CounterState = {
    value: 0
}

export const counterSlice = createSlice({
    name: "Counter", //
    initialState,
    reducers: {
        increment: (prevState) => { //
            prevState.value += 1
        },
        decrement: (prevState) => { //
            prevState.value -= 1
        },

    }
})

export const { increment } = counterSlice.actions
export default counterSlice.reducer
```


* A "slice" is a collection of Redux reducer logic and actions for a single feature in your app, typically defined together in a single file. The name comes from splitting up the root Redux state object into multiple "slices" of state.





# nextjs SSR redux

1. SSR getServerSideProps() fetches stuff from backend and fills the store
2. SSR sends the store to the clientside as props
3. clientside takes the props to initialize it's own store
<!--  -->
* very loose psuedocode
```{.js filename=somepage.js}
export function getServerSideProps() {
  const reduxStore = initializeStore()//create empty store
  const data = fetch data from backend
  dispatch({data})
  return { props: { initialReduxState: reduxStore.getState() } }
}
```

```{.js filename=pages/_app.js}
import { Provider } from 'react-redux'
import { useStore } from '../store'

export default function App({ Component, pageProps }) {
  // here we restore the state created by `getServerSideProps`
  // it's expected that all routes always return this prop
  const store = useStore(pageProps.initialReduxState)

  return (
    <Provider store={store}>
      <Component {...pageProps} />
    </Provider>
  )
}
```