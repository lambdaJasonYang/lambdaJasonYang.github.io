---
title: Haskell prog lang
tags: musings
---

# Overview

* Type-Checker: Takes code as input, Returns Boolean based on whether code has consistent typing
* verifier: Takes code as input, Returns Boolean based on whether some invariant is satisfied
* Compiler: Converts code in one lang to code in another lang

# Tokenizer
Converts code string to tokens

# Parser

Converts sets of tokens to AST.

```hs
data Day = Monday 
          |Tuesday
          |Wednesday
          |Thursday
          |Friday
          |Saturday
          |Sunday
          deriving (Show)
next_weekday :: Day -> Day 
next_weekday d = case d of Monday -> Tuesday
                           Tuesday -> Wednesday
                           Wednesday -> Thursday
                           Thursday -> Friday
                           Friday -> Saturday
                           Saturday -> Sunday
                           Sunday -> Monday
:t next_weekday Friday

next_weekday Friday
next_weekday (next_weekday Saturday)

```

```hs
-- untyped lambda calculus values are functions
data Value = FunVal (Value -> Value)

-- we write expressions where variables take string-based names, but we'll
-- also just assume that nobody ever shadows names to avoid having to do
-- capture-avoiding substitutions



data Expr
  = Var String
  | Apply Expr Expr
  | Lam String Expr

-- We model the environment as function from strings to values, 
-- notably ignoring any kind of smooth lookup failures
type Env = Name -> Value

-- The empty environment
env0 :: Env
env0 _ = error "Nope!"

-- Augmenting the environment with a value, "closing over" it!
addEnv :: Name -> Value -> Env -> Env
addEnv nm v e nm' | nm' == nm = v
                  | otherwise = e nm

-- And finally the interpreter itself
interp :: Env -> Expr -> Value
interp e (Var name) = e name          -- variable lookup in the env
interp e (App ef ex) =
  let FunVal f = interp e ef
      x        = interp e ex
  in f x                              -- application to lambda terms
interp e (Abs name expr) =
  -- augmentation of a local (lexical) environment
  FunVal (\value -> interp (addEnv name value e) expr)
```

```hs
--A faithful haskell implementation of Pierce's Types and Programming Languages TAPL book for Untyped ARITH Ch 3
--without using external Haskell libraries or advanced Haskell constructs.


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

--------------------------------------------------------------------- Small Step Evaluator

-- if true then t2 else t3 -> t2                    {E-IfTrue}

-- if false then t2 else t3 -> t3                   {E-IfFalse}

--                     t1 -> t1'
-- ------------------------------------------------  {E-If}
--  if t1 then t2 else t3 -> if t1' then t2 else t3


--       t1 -> t1' 
-- -------------------           {E-Succ}
--  succ t1 -> succ t1'

-- pred 0 -> 0                   {E-PredZERO}

-- pred (succ nv1) -> nv1        {E-PredSucc}

--        t1 -> t1'
-- ----------------------        {E-Pred}
--   pred t1 -> pred t1'

-- iszero 0 -> true              {E-IsZeroZERO}

-- iszero (succ nv1) -> false    {E-IsZeroSucc}

--         t1 -> t1'
-- -------------------------     {E-IsZero}
--  iszero t1 -> iszero t1'

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


--------------------------------------------------------------------- Big Step Evaluator

-- v=>v                          {B-Value}

-- t1 => true   t2 => v2 
-- ----------------------------  {B-IfTrue}
--  if t1 then t2 else t3 => v2

--    t1 => false   t3 => v3
-- ----------------------------  {B-IfFalse}
--  if t1 then t2 else t3 => v3

--       t1 => nv1
-- --------------------          {B-Succ}
--  succ t1 => succ nv1

--       t1 => 0
-- --------------------          {B-PredZERO}
--    pred t1 => 0

--    t1 => succ nv1
-- --------------------          {B-PredSucc}
--    pred t1 => nv1

--        t1 => 0
-- --------------------          {B-IsZeroZERO}
--   iszero t1 => true

--    t1 => succ nv1
-- --------------------          {B-IsZeroSucc}
--   iszero t1 => false

-- Why I was forced to fuse two case patterns into 1 for {B-PredZERO} {B-PredSucc} and also for {B-IsZeroSUCC} {B-IsZeroZERO}
-- bigstep_eval ( S $ P $ O ) would get recursively stuck on the earliest {B-PredSucc} pattern 
-- bigstep_eval ( P $ S $ O ) would get recursively stuck on the earliest {B-PredZERO} pattern 
-- My original intent to keep faithful was to have each evaluation rule be represented by it's own case pattern.

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

```hs
data Term =
    TmTrue 
  | TmFalse 
  | TmIf Term Term Term
  | TmZero 
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Show, Eq)

eval :: Term -> Term
eval t = case eval1 t of
  -- I'm not sure but it seems as though this is actually broken
  -- in Pierce's original implementation so I've added "if t' == t .."
  Just t' -> if t' == t then t else eval t'
  Nothing -> t

-- True if the given term is a numerical value. In short a Peano natural number
-- consisting of a chain of calls to succ terminating at a zero. Anything else
-- is either a type error or needs to be evaluated.
isNumericVal :: Term -> Bool
isNumericVal t = case t of
  TmZero -> True
  TmSucc t1 -> isNumericVal t1
  _ -> False

-- True if the given term represents a fully evaluated value.
isVal :: Term -> Bool
isVal t = case t of
  TmTrue -> True
  TmFalse -> True
  t -> isNumericVal t

-- Evaluate the given term one step.
eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 t3) = return t2
eval1 (TmIf TmFalse t2 t3) = return t3
eval1 (TmIf t1 t2 t3) = do
  t1' <- eval1 t1
  return $ TmIf t1' t2 t3
eval1 (TmSucc t1) = fmap TmSucc t1'
  where t1' = eval1 t1
eval1 (TmPred TmZero) = return TmZero
eval1 (TmPred (TmSucc nv1))
  | isNumericVal nv1 = return nv1
eval1 (TmPred t1) = fmap TmPred t1'
  where t1' = eval1 t1
eval1 (TmIsZero TmZero) = return TmTrue
eval1 (TmIsZero (TmSucc nv1))
  | isNumericVal nv1 = return TmFalse
eval1 (TmIsZero t1) = fmap TmIsZero t1'
  where t1' = eval1 t1
eval1 _ = Nothing

eval1 (TmSucc $ TmPred $ TmSucc $ TmZero)
```