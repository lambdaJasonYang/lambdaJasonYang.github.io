---
title: Overcomplicating Memory Model and Pointers
tags: musings, prog
---

### The Problem
Online forums and tutorials tend to introduce C pointers and memory model as some complex behemoth.
Professors introduce the memory model with a bucketload of terms like stacks, heaps, address space, registers.

### Solution
To understand pointers we must understand memory.
Here is a simple way to describe memory which makes me surprised why no one has taught it or introduced it this way.
Let's just do alway with all the complex OS terminology.

>Computer memory is just an array.

The memory addresses are indexes of the array.  
The values that memory addresses point to are the elements of the array.  

Clearly we know how to read the value at memory address.
Translated: Clearly we know how to read the element at each array index.

A computer isn't a storage device, it works by being dynamic which includes changing memory values.
Translated: So how can we change values of this array?  

Pointers! 

See everything is just part of the array even pointers.
BUT pointers are given a unique function we call "dereference".
a[3] = 6
a[6] = 1
Let's say "3" is a pointer.
then dereference(3) = a[6] => 1


So how do 
$$ \bbox[5px, border: 2px solid red]{x} \bbox[5px, border: 2px solid red]{x} $$