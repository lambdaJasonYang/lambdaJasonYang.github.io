---
title: C Pointers
tags: prog, OS
---

# Confusion

Big confusion, Initialization syntax and dereference syntax look really similar  

```C
int* a; //Initialization syntax
*a; //Dereference syntax
```
Have the `*` hug the type `int` to indicate Initialization `int*`

# Notation

* Pointers should be thought of as a singleton array.
* Dereference is just getting the element of this singleton.

```C
int * a; how about [$]
print *a; how about print [$][0]
```

But this is confuses us with array so instead lets use  
`int* a := <int>`  
`   *a := !<int> = int`

! for dereference  
<> for pointer  

## Reading pointers

```C
//pointer to array of pointers
int* (* a)[5]

<[..,<int>]>
```

string is just a pointer to a char.

```C
<char>
```

# Memory model

>Computer memory is just an array.

We call the computer one long array MEM.  

The memory addresses are indexes of the array.  
The values that memory addresses point to are the elements of the array.  

To emulate the dynamic state of a computer we use pointers to change the values of the array.

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

# Registers


Just think of these registers as powerful as pointers.
someaddr = 3


the assembly notation `mov [eax],3` is equal to MEM[EAX] set to 3 

