---
title: Operating Systems
tags: prog, OS
toc: y
---

# Section 1

## Terms 

* **process** - Protected **address space** + 1 or more **threads**
* **thread** - virtual cores
  * Program coutner
* **unprivileged instruction** - like `ADD` or `MOV` which read/write to memory that both the user and kernel level can use.
* **privileged instruction** - Kernel mode instructions like enabling/disabling **interrupts**. If a user could disable interrupts, user process could run on hardware thread indefinitely.