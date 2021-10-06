---
title: Algorithm Problem Sets

tags: tech, prog, asm, C
---

### Documenting some random algorithmic problems:

---

>Describe an algorithm that given a linked list,  
if odd returns the middle element,  
if even length then return the first of the two middle elements.

* Solve with 2 pointers, state of subproblem isomorphic with size of partition of 1st pointer allowing induction.
* 1st pointer moves 2-spaces
  * 1st pointer partitions the list to a subproblem
* 2nd pointer moves 1-space
  * 2nd pointer is IH invariant to each subproblem.  
  2nd pointer represents the solution or midpoint of the list.

---

>Design recursive procedure that takes n::int and returns ALL n-length lists where all consecutive letters are distinct. Only letters {A,B,C} can be used.   
Eg. 'AB' 'AC' 'BA' 'BC' 'CA' 'CB' 

* IH: For (n-1) assume we have solution::[[str]] containing  
list of (n-1)-lists = {'A..','B..','C..',...}
* For each (n-1)-list in IH check head of the (n-1)-list then build new lists from the 2 letters that do not match the head.  
Eg: if 'A...' we add B or C to get new combinations {'BA..','CA..'}

Misconception:

* Why not also check for tail of (n-1)-list? 
  * combinations would repeat giving us double the solutions. 
    * Assume {'A..C','B..A'} $\in$ IH 
    * Append to B,C to head 'A..C' => {**'BA..C'**,'CA..C'}
    * Append to B,C to tail 'B..A' => {**'B..AC'**,'B..AB'}
    * Notice 'BA..C' and 'B..AC' are the same

---

>4-pole Hanoi. Given 4 poles A,B,C,D clockwise in a square, you can only move clock wise.  Design a method to move n::int disk from A to B.

A--B  
|\ \  |  
D--C  

* IH: Assume we can move (n-1) disk to neighboring pairs like (A,B), (B,C) ...
* Keypoints 
  * We can still use IH on the (n-1) disks after moving the largest bottom disk,  
  since the largest disk does not cause any side effect or reduce degree of freedom.
* move top (n-1) disks from A to B. [IH]
  * [ A={n}, B={n-1,n-2..}]
* move all (n-1) disks from B to C. [IH]
  * [ A={n}, B={} , C={n-1,n-2,...}]
* move all (n-1) disks from C to D. [IH]
  * [ A={n}, B={} , C={}, D={n-1,n-2,...}]
* move single largest disk from A to B.
  * [ A={}, B={n}, D={n-1,n-2,...}]
* move all (n-1) disks from D to A. [IH]
  * [ A={n-1,n-2,...} , B={n}  ]
* move all (n-1) disks from A to B. [IH]
  * [  B={n,n-1,n-2...}  ]

---

> Take Tree T as an input. For every node C, output the shortest path from C to the closest leaf.

### TwoSum

```bash
#Psuedocode
Goal: Return INDEX of nums that sum to target

nums = [2,7,11,15]
hmap = {Value#1stpartialSum |-> Index#2ndpartialSum} 
also   {Value#2ndpartialSum |-> Index#1stpartialSum}
     where Invariant is Value#1stpartialSum + Value#2ndPartialSum = target
let nums = Value#1stpartialSum
enumerate(nums) = {index |-> nums} where i::index, k::Value#1stpartialSum
check k::Value#1stpartialSum in hmap
    true => return [hmap(k)::Index#2ndpartialSum, (index of k::Value#1stpartialSum)::Index#1ndpartialSum
    false => SET hmap[target - Value#1stpartialSum |-> Index#2ndpartialSum]

```


```{.python .numberLines} 
class Solution(object):
    def twoSum(self, nums, target):
        hmap = {}
        for Index1stPartial,Value1stPartial in enumerate(nums):
            if Value1stPartial in hmap:
                Index2ndPartial=hmap[Value1stPartial]
                return [Index2ndPartial,Index1stPartial]
            Value2ndPartial = target-Value1stPartial
            hmap[Value2ndPartial] = Index1stPartial
        return []

#Test = Solution().twoSum([2,7,11,15],9)
#print(Test)

```

the target is composed  of values from two partial sums  (Value#1stpartialSum + Value#2ndpartialSum)


```go
func twoSum(nums []int, target int) []int {
	hmap := make(map[int]int)
	for Index1stPartial, Value1stPartial := range nums {
		if _, exists := hmap[Value1stPartial]; exists {
			Index2ndPartial := hmap[Value1stPartial]
			return []int{Index2ndPartial, Index1stPartial}
		} else {
			Value2ndPartial := target - Value1stPartial
			hmap[Value2ndPartial] = Index1stPartial
		}

	}
	return []int{}
}
```

2 cases:

Hashmap Miss

```plantuml
@startuml
scale 0.6
frame nums {
    usecase "Index,Value#1stPartialSum" as UC1
    usecase "Index,Value#2ndPartialSum" as X
}

frame HashValue {

  usecase "Can't find Index#2ndPartialSum" as AC1
  usecase "Index#1stPartialSum" as AC2
}
frame HashIndex {
usecase "Can't find Value#1stPartialSum" as UC3
  usecase "Value#2ndPartialSum" as UC2

}

@enduml

UC1 -[dotted]-> UC3 : "1) hashmap miss\n Line 20 False"
UC3 -[dotted]-> AC1 : "1) hashmap miss\n Line 20 False"
UC2 --> AC2 :  "3) fill hashmap\n Line 24"
UC1 --> UC2 : "2) target - Value#1st Line 23"

```

Hashmap hit 

```plantuml
@startuml
scale 0.6
frame nums {
    usecase "Index,Value#1stPartialSum" as X
    usecase "Index,Value#2ndPartialSum" as UC1
}

frame HashValue {
  
  usecase "Index#1stPartialSum" as AC2
}
frame HashIndex {
  usecase "Value#2ndPartialSum" as UC2

}

usecase "return SolutionIndexes" as SS
UC1 --> SS
AC2 --> SS
@enduml

UC2 --> AC2 :  "3) Solution \n Line 22"
UC1 --> UC2 : "1) hashmap hit\n Line 20 True"

```
---


### Container with most water