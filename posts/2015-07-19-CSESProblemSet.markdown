---
title: CSES ProblemSets

tags: tech, prog, asm, C
---


### TwoSum

```code
Goal: Return INDEX of nums that sum to target

nums = [2,7,11,15]
hmap = {Value#1stpartialSum |-> Index#2ndpartialSum} 
also   {Value#2ndpartialSum |-> Index#1stpartialSum}
     where Invariant:: Value#1stpartialSum + Value#2ndPartialSum = target
let nums = Value#1stpartialSum
enumerate(nums) = {index |-> nums} where i::index, k::Value#1stpartialSum
check k::Value#1stpartialSum in hmap
    true: return [hmap(k)::Index#2ndpartialSum, (index of k::Value#1stpartialSum)::Index#1ndpartialSum
    false: SET hmap[target - Value#1stpartialSum |-> Index#2ndpartialSum]

```


```{.python .numberLines} 
class Solution(object):
    def twoSum(self, nums, target):

        '''
Goal: Return INDEX of nums that sum to target

nums = [2,7,11,15]
hmap = {Value#1stpartialSum |-> Index#2ndpartialSum} 
also   {Value#2ndpartialSum |-> Index#1stpartialSum}
     where Invariant:: Value#1stpartialSum + Value#2ndPartialSum = target
let nums = Value#1stpartialSum
enumerate(nums) = {index |-> nums} where i::index, k::Value#1stpartialSum
check k::Value#1stpartialSum in hmap
    true: return [hmap(k)::Index#2ndpartialSum, (index of k::Value#1stpartialSum)::Index#1ndpartialSum
    false: SET hmap[target - Value#1stpartialSum |-> Index#2ndpartialSum]
'''

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
next step  
Hashmap hit 

```plantuml
@startuml

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