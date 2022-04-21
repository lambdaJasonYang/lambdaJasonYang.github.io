---
title: Lessons from Haskell
tags: prog
toc: y
---

# Designing Data types

Design a tic tac toe board

1. Naive data structure, 9 spots = 9 constructor product type = struct or record type
2. Observe that $3^2=9$ and a algebraic exponential type is a function type `2 -> 3` aka `(X-axis, Y-axis) -> (1st,2nd,3rd)`
