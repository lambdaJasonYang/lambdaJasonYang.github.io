---
title: TAPL Untyped Arith
tags: mathcs, musings, functional, prog
---

A faithful haskell implementation of Pierce's Types and Programming Languages TAPL book for Untyped ARITH Ch 3
without using external Haskell libraries or advanced Haskell constructs.


```hs
data Term
    = T --True
    | F --False
    | O --ZERO
    | IfThenElse Term Term Term
    | S Term --Succ
    | P Term --Pred
    | IsZ Term --IsZero
    | Error 
    deriving (Show)

isNumericVal :: Term -> Bool
isNumericVal t = case t of O -> True
                           S t -> isNumericVal t
                           _ -> False
isNumericVal T -- Output: True 
isNumericVal (S O) -- Output: False

isVal :: Term -> Bool
isVal t = case t of T -> True
                    F -> True
                    t1 -> if (isNumericVal t1) then True else False
                    _ -> False
```
```{.hs group="smallstep" glabel="hs"}
eval :: Term -> Term 
eval t = case t of 
               IfThenElse T t2 t3 -> t2 --{E-IfTrue}
               IfThenElse F t2 t3 -> t3 --{E-IfFalse}
               IfThenElse t1 t2 t3 -> let t1' = eval t1 in IfThenElse t1' t2 t3 --{E-If}
               S t1 -> let t1' = eval t1 in S t1'  --{E-Succ}
               P O -> O  --{E-PredZERO}
               P (S nv1) -> if (isNumericVal nv1) then nv1 else Error --{E-PredSucc}
               P t1 -> let t1' = eval t1 in P t1'  --{E-Pred}
               IsZ O -> T  --{E-IsZeroZERO}
               IsZ (S nv1) -> if (isNumericVal nv1) then F else Error --{E-IsZeroSucc}
               IsZ t1 -> let t1' = eval t1 in IsZ t1' --{E-IsZero}
               _ -> Error
                   
eval (O)   -- Output: Error             
--  apparently the evaluator from the book did not account for evaluating just ZERO 
eval (S $ S $ O) -- Output: S ( S Error)

eval (P $ S $ S $ O) -- Output: S O
-- we are forced to insert a Pred 'P' somewhere for it to properly evaluate
```
```{.txt group="smallstep" glabel="rules"}
------------------------------------------------------------------- Small Step Evaluator

if true then t2 else t3 -> t2                    {E-IfTrue}

if false then t2 else t3 -> t3                   {E-IfFalse}

                    t1 -> t1'
------------------------------------------------  {E-If}
 if t1 then t2 else t3 -> if t1' then t2 else t3


      t1 -> t1' 
-------------------           {E-Succ}
 succ t1 -> succ t1'

pred 0 -> 0                   {E-PredZERO}

pred (succ nv1) -> nv1        {E-PredSucc}

       t1 -> t1'
----------------------        {E-Pred}
  pred t1 -> pred t1'

iszero 0 -> true              {E-IsZeroZERO}

iszero (succ nv1) -> false    {E-IsZeroSucc}

        t1 -> t1'
-------------------------     {E-IsZero}
 iszero t1 -> iszero t1'
```

* Why I was forced to fuse two case patterns into 1 for {B-PredZERO} {B-PredSucc} and also for {B-IsZeroSUCC} {B-IsZeroZERO}
  * `bigstep_eval ( S $ P $ O )` would get recursively stuck on the earliest {B-PredSucc} pattern 
  * `bigstep_eval ( P $ S $ O )` would get recursively stuck on the earliest {B-PredZERO} pattern 
* My original intent to keep faithful was to have each evaluation rule be represented by it's own case pattern.

```{.hs group="bigstep" glabel="hs"}
bigstep_eval :: Term -> Term

bigstep_eval t = case t of 
                       T -> T --{B-Value}
                       F -> F --{B-Value}
                       O -> O --{B-Value}
                       (IfThenElse t1 t2 t3) -> case (bigstep_eval t1) of 
                                                               T -> let v2 = (bigstep_eval t2) in v2 --{B-IfTrue}
                       (IfThenElse t1 t2 t3) -> case (bigstep_eval t1) of 
                                                               F -> let v3 = (bigstep_eval t3) in v3 --{B-IfFalse}
                       (S t1) -> let nv1 = (bigstep_eval t1) in (S nv1) --{B-Succ}
                       (P t1) -> case (bigstep_eval t1) of 
                                                       (S nv1) -> nv1 --{B-PredSucc}
                                                       O -> O --{B-PredZERO}                                                       
                       (IsZ t1) -> case (bigstep_eval t1) of 
                                                       (S nv1) -> F --{B-IsZeroSucc}
                                                       O -> T  --{B-IsZeroZERO}                                             

                       -- Had to fuse the two case patterns below and the do the same with {B-IsZeroSucc} {B-IsZeroZERO}
                       -- (P t1) -> case (bigstep_eval t1) of 
                       --                                 (S nv1) -> nv1 --{B-PredSucc}
                       -- (P t1) -> case (bigstep_eval t1) of 
                       --                                 O -> O --{B-PredZERO}                       
                           
bigstep_eval (IfThenElse T F T) 
bigstep_eval ( P ( S $ S $ O))
bigstep_eval (S $ P $ O)
bigstep_eval (IsZ O)
```

```{.txt group="bigstep" glabel="rules"}
------------------------------------------------------------------- Big Step Evaluator

v=>v                          {B-Value}

t1 => true   t2 => v2 
----------------------------  {B-IfTrue}
 if t1 then t2 else t3 => v2

   t1 => false   t3 => v3
----------------------------  {B-IfFalse}
 if t1 then t2 else t3 => v3

      t1 => nv1
--------------------          {B-Succ}
 succ t1 => succ nv1

      t1 => 0
--------------------          {B-PredZERO}
   pred t1 => 0

   t1 => succ nv1
--------------------          {B-PredSucc}
   pred t1 => nv1

       t1 => 0
--------------------          {B-IsZeroZERO}
  iszero t1 => true

   t1 => succ nv1
--------------------          {B-IsZeroSucc}
  iszero t1 => false

```

# Analysis

>Translate operational semantics to real code

```hs
data Term := T | F | O | IfThenElse Term Term Term | S Term | P Term | IsZ Term | Error 
```
```text
    2  
------------
  1 -> 3
```

**Placeholder vars** - Variables that are NOT constructed terms (in this case T,F,O, IfThenElse ...) and are NOT bound by pattern matching.

1. Pattern matching: `succ t1`
2. Rules To satisfy: 
    * Check the placeholder variables: only `t1'` ( Note:`t1` is not a placeholder since it is pattern matched)
    * The only rule is evaluation of `t1`.
    * Since `Bound --evals--> Free` it is trivial to just set the Free variable as the evaluated result using `let t1' = ...`.
3. Return: Succ applied to placeholder var `t1'`

```hs
--      t1 -> t1' 
-- -------------------           {E-Succ}
--  succ t1 -> succ t1'

--   ---------->------------>------
--  |                              |
--succ t1      t1 --> t1'       succ t1'       
 S t1 -> let t1' = eval t1 in S t1'  --{E-Succ}

```

```hs
--      Evaluate[t1] --rule-> t1' 
-- ---------------------------------------           
--  Pattern Match[succ t1] -> Result: succ t1'
```




Operational Semantics

```hs
-- t1 => true   t2 => v2 
-- ----------------------------  {B-IfTrue}
--  if t1 then t2 else t3 => v2

(IfThenElse t1 t2 t3) -> case (bigstep_eval t1) of T -> let v2 = (bigstep_eval t2) in v2
```
                                                               
1. Pattern match `if t1 then t2 else t3`
2. Above the line are rules to satisfy. 
   * Notice we only have 1 placeholder var `v2`
   * `true` is NOT a placeholder.
   * Constraints 
      * `t1 ===eval===> true` evaluated t1 must be true constraint
      * `t2 ===eval===> v2` t2 is trivially evaluated and stored into the placeholder var
3. Return: return the placeholder var
