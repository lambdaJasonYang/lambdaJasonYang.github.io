---
title: Very Long Lean
tags: prog
toc: y
---

```hs
variables (P Q R : Prop)
-- Here is a proof which does not use tactics at all, but uses lambda calculus.
-- It is called a "term mode" proof. We will not be discussing term mode
-- much in this course. It is a cool way to do basic logic proofs, but
-- it does not scale well in practice.
example : P → ¬ (¬ P) :=
λ hP hnP, hnP hP

-- This one cannot be proved using constructive mathematics!
-- You _have_ to use a tactic like `by_contra` (or, if you're happy
-- to cheat, the full "truth table" tactic `tauto!`.
-- Try it without using these, and you'll get stuck!
theorem double_negation_elimination : ¬ (¬ P) → P :=
begin
  sorry,
end

```

```hs

def C : (A → β → C) → β → A → C :=
λg b a, g a b

/-! Let `D` := `g : A → β → C, b : β, a : A`. We have

    ------------------ Var    ---------- Var
    D ⊢ g : A → β → C         D ⊢ a : A
    ------------------------------------ App    ---------- Var
    D ⊢ g a : β → C                             D ⊢ b : β
    ------------------------------------------------------ App
    D ⊢ g a b : C
    ---------------------------------------------- Lam
    g : A → β → C, b : β ⊢ (λa : A, g a b) : A → C
    ----------------------------------------------------- Lam
    g : A → β → C ⊢ (λ(b : β) (a : A), g a b) : β → A → C
    --------------------------------------------------------------------- Lam
    ⊢ (λ(g : A → β → C) (b : β) (a : A), g a b) : (A → β → C) → β → A → C -/
```

```hs
def more_nonsense : ((A → B) → C → D) → C → B → D :=
λ f c b, (f (λ (ha:A ),b) ) c

    λ f c b
      |
      @
   /  |     \     
  f (λha.b)   c

λ f c b ⊢ f (λha.b) c 
--notice how we don't have (A->B) so we must build it by abstracting a lambda from B.
--proof theoretically, if we have a proof of B we can create a proof of A implies B.
B -| (A -> B)
λb -| λ(ha:A ),b
```

$$\cfrac{\vdash b}{ha \vdash b}$$

::: {.hobbies group="hi" glabel="do"}
hafe
:::

::: {.hobbies group="hi" glabel="do"}
hafe
:::

::: {.hobbies group="hi" glabel="do"}
hafe
:::

::: {.hobbies group="hi2" glabel="do2"}
hafe
:::

::: {.hobbies group="hi2" glabel="do2"}
hafe
:::

::: {.hobbies group="hi2" glabel="do2"}
hafe
:::