---
title: Overcomplicating Memory Model and Pointers
tags: prog, OS
---

### The Problem
Online forums and tutorials tend to introduce C pointers and memory model as some complex behemoth.
Professors introduce the memory model with a bucketload of terms like stacks, heaps, address space, registers.

### Solution
To understand pointers we must understand memory.
Here is a simple way to describe memory which makes me surprised why no one has taught it or introduced it this way.
Let's just do alway with all the OS terminology.

We will design an very simplified model that I will name the EZ model . For a non-low level programmer. 

>Computer memory is just an array.

Let's call this array MEM.

In our EZ model, we start with one array, MEM.

The memory addresses are indexes of the array.  
The values that memory addresses point to are the elements of the array.  

Clearly we know how to read the value at memory address.
Translated: Clearly we know how to read the element at each array index.

A computer isn't a storage device, it works by being dynamic which includes changing memory values.
Translated: So how can we change values of this array?  

Pointers! 

See everything is just part of the array even pointers.
BUT pointers are given a unique function we call "dereference".
$$ \underset{\downarrow}{\bbox[5px, border: 2px dotted purple]{P} } \ \ \ \ \ $$
$$ \underset{\downarrow}{\bbox[5px, border: 2px solid green]{0} } \underset{\downarrow}{\bbox[5px, border: 2px solid green]{1}}  \underset{\downarrow}{\bbox[5px, border: 2px solid green]{2}}  \underset{\downarrow}{\bbox[5px, border: 2px solid green]{3}}   $$
$$ \bbox[5px, border: 2px solid red]{7} \bbox[5px, border: 2px solid red]{2} \bbox[5px, border: 2px solid red]{6} \bbox[5px, border: 2px solid red]{4}$$

The image above is our array MEM.  
The green boxes indicate the indexes, like any array it starts at 0.  
The red boxes indicate the values of our array.  

In OS language, the green boxes indicate memory addresses.
The red boxes are the value that the memory addresses refer to.
``` C    
//filling our MEM, aka loading a program into memory
MEM[0] = 9   
MEM[1] = 2
MEM[2] = 6
MEM[3] = 4  

//dereference function
Dereference(MEM[1])
//This will output 6 
```
Calling Dereference(MEM[1]) can be reduced to Dereference(2), which can be reduced to MEM[2] which is just 6.

Dereference(MEM[1]) == MEM[MEM[1]]

Let's say "3" is a pointer.
then dereference(3) will return 1
For the intents of this section of understanding pointers, let's just assume we can magically determine which indexes of MEM are pointers.

### EZ2 Model
In our EZ model, we magically decided which indexes are pointers which is not correct.
Pointers are a implementation in assembly that uses registers.
The functionality of pointers to a C programmer is analogous to the functionality of registers to a assembly programmer.

The real pointers in computers aren't those "int* x" that programmers code up but computer registers like eax, ebx...
Just think of these registers as powerful as pointers.
someaddr = 3

Let's clear things up, we call the assembly model and the EZ Model.

the assembly notation [EAX] notationally equivalent to our EZ MEM[EAX] 

How pointers behave to C programmers is analogous to how registers work to assembly programmers.
