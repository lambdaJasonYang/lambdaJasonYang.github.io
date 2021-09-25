---
title: Data Struct - Balanced Trees
tags: mathcs, logic
---
### Regular BST

```node
          ^                   50
          |               /        \
          |           25              75
 height=3 |         /    \          /    \
  n=15    |       10     30        60     90
          |      /  \   /  \      /  \   /  \
          V     4   12 27  40    55  65 80  99
```

###  RB Tree

* root is black
* null nodes are black (Children of the leafs)
* If a node is red, it's children are black



Left Rotation 
```code
      x              y
     / \            / \
    a   y    =>    x   c
       / \        / \  
      b   c      a   b   
```

Right Rotation
```code
      y              x
     / \            / \
    x   c    =>    a   y
   / \                / \
  a   b              b   c
```


Recolor