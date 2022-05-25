---
title: What is Pattern matching?
tags: mathcs, functional, categorytheory, puremath
---

# Summary

```hs
def somefunc : ... :=
match (FOCUS_ON_RESULTANT_TYPE) with
| (Term-Intro_RESULTANT_TYPE_1) = > Anything
| (Term-Intro_RESULTANT_TYPE_2) = > Anything
| (Term-Intro_RESULTANT_TYPE_3) = > Anything
...

```

* `List.get? xs 1` is our focus
* RESULTANT_TYPE of `List.get? x 1` is `Option Nat`
* Term introduction rules of the RESULTANT_TYPE
  * Term-Intro 1 for `Option Nat` is constructor function Option.none
  * Term-Intro 2 for `Option Nat` is constructor function Option.some n

```hs
#check @List.get? -- (List × Nat) -> Option Nat 

def stringify (xs : List Nat ) : Option String :=
match List.get? xs 1 with 
| (Option.none    : Option Nat) => ...
| (Option.some n : Option Nat) => ...
```


1. Look at the match `match List.get? xs 1 with`
    * we only care about the resultant type of the match function `List.get?` which is `Option Nat`
    * `Option Nat` is deconstructed from it's inductive term introduction
2. 


# Extra

$$ \cfrac{String}{(Maybe\ String)} \qquad \cfrac{a \quad Tree\ a \quad Tree\ a}{(Node\ a\ (Tree\ a)\ (Tree\ a))}$$


Pattern matching gives us free deconstructive functions that breaks down inductive datatypes.  
Our goal is to make the implicit pattern-matching explicit.

Notation @ for forward composition

```hs
login:: String -> Maybe String
password :: String

login password :: String @ (String -> Maybe String )
login password :: Maybe String

-- @ represents forward function composition
-- x@f@g means g(f(x)) 
```


```hs
case (Just x :: Maybe String) of
    Nothing -> Just "0"
    Just x -> Just ("3" ++ x)

--lets deconstruct the type of "case", it takes in a "Just x" meaning our first type is a "Maybe String" and outputs either Just '0' or Just x++'3' meaning it's output type is also "Maybe String"
case :: Maybe String -> Maybe Int
         Nothing     ->  Just 0
         Just x      ->  Just (1+Int x)  --(this is represented as the chain of compositions shown below)
         (patternMatch (Just x)) @ (Int _) @ (1+ _ ) @ (Just _) --reduces to
                              x  @ (Int _) @ (1+ _ ) @ (Just _)
                                   (Int x) @ (1+ _ ) @ (Just _)
                                            (1+Int x)@ (Just _)
                                                       (Just 1+Int x) 

--however what allows us to pull out that "x" and rebuild it into ("3" ++ x)?
--This is the trivial pattern match function we get when we use a case 
patternMatch :: Maybe String -> String
patternMatch (Just x) = x
```
```hs
     patternMatch      @  Int_Convert  @   AddOne   @   MaybeConstructor :: 
Maybe String -> String @ String -> Int @ Int -> Int @ String -> Maybe String

--see how we can represent are case function as a chain of compositions
case == patternMatch @ Int_Convert @ AddOne @ MaybeConstructor
```

Takeaway

* Whenever we use `case :: X -> _` 
  * we get a free `patternMatch :: X -> Deconstructed X`
* Our example we had `case :: Maybe String -> _` so we get a free `patternMatch :: Maybe String -> String`

## Trees

Lets continue with the pattern match functions for tree paradigm.

```hs
data Tree a = Leaf | Node a (Tree a) (Tree a)

patternmatch1 :: Tree a -> a
patternmatch2 :: Tree a -> Tree a
patternmatch3 :: Tree a -> Tree a

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node val left right) = Node (f val) (mapTree f left) (mapTree f right)

--converted from implicit pattern matching to explicit pattern matching
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node val left right) = Node (f patternmatch1 (Node val left right)) (mapTree f patternmatch2 (Node val left right)) (mapTree f patternmatch3 (Node val left right))
```

`Node a (Tree a) (Tree a)` is a product type.

```text
a    Tree a     Tree a
\     |        /  
 |    |      /   
  |   |    /    
  | ProductType
   |  |  /        
    \ | /          
      *
```


Pattern matching is powerful and it gives us free canonical projections of product types.

Pattern matching lets us deal with Sum types similar to Logical OR-elim
  * By dealing with each Case/Constructor of the Disjunction/Sum type with multiple Implications/Functions by case-analysis/pattern-matching


