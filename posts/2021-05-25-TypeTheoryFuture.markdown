---
title: Type Theory and should you care
tags: musings
---

Behind all the hype with Machine learning (GPT-3, OpenAI) there is another contender that may rock your world, 
**Dependent Type Theory**!!

#### Prediction

New tech may allow us to "simply" design code that carries a proof of correctness using types and the compiler will automagically swap out our inefficient code with the most optimized version.  
I say "simply" tongue in cheek because these proofs are actually really bulky in comparison to traditional mathematical proofs but it may change in the future with better designed tactics(proof writing functions).

#### Present

Many functional programming languages already implements some form of dependent type theory like Haskell, Coq or Lean or even templates in C++.  
However they are more often than not unpractical in the  real world (except Hakyll which this site is powered with).  

People can design proof of correctness in Haskell or C++ but I find that it does not actually help in solving problems but rather it shifts the problem towards the manipulation of types.

#### How does it affect you?

Most likely it won't if you aren't a compiler designer or working in high fidelity/high security software that requires software verfication. 
Perhaps people will implement features of dependent type theory in newer languages like Rust but learning the theoretic underpinnings of type theory isn't necessary to start programming with powerful dependent types.