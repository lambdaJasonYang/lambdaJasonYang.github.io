---
title: Closures in JS, Python
tags: prog, cloud, frontend
---


# Lambda

$$\lambda x. funcbody$$

"lambda x" is a binder whose scope is "funcbody". An occurance of variable x is bound when it occus in "funcbody" of a "lambda x. funcbody"



* `b` is shown in the function argument and the global scope but they will NEVER refer to the same `b`. We can pretend that the function always renames conflicting names from `b` to `b_frame`.
    * Technically the reason they never refer to the same object, is because the global b lives in the global frame and when f is called another stackframe is placed so that the argument b lives in the function stack frame.

```py
def f(b): #stackframe
  b = 4
  return ..
b = 6  #globalscope
```

```py
def f(b_frame1): 
  b_frame1 = 4
  return ..
b = 6  #globalscope
```

```text
frames            objects
---
globalframe 
b ------------>     6
---
f frame
b ------------>     4
---
```