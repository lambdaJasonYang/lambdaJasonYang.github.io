---
title: Quirky Python Traps
tags: prog, QuickCode, python
toc: y
---

# 2d array

```py
dp2 = [[99999] * 3] * 4
dp2[0][0] = 1
#you expect it to change one cell but WRONG
[[1, 99999, 99999], 
[1, 99999, 99999], 
[1, 99999, 99999], 
[1, 99999, 99999]]
```

The list object we actually created is **4 references to the same 3-length list**

```py
1 --> listX: [99999,99999,99999]
2 --> listX: [99999,99999,99999]
3 --> listX: [99999,99999,99999]
4 --> listX: [99999,99999,99999]
```

`dp2[0][0]` causes to change listX object which means all 4 references now point to a changed object.   
