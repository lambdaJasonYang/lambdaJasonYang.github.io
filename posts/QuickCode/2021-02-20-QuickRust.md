---
title: Quick Rust
tags: prog, QuickCode
toc: y
---

# Cargo

```bash
cargo new helloworld
cargo run helloworld
```

# Rust

* Rust has no classes
* Rust mainly uses struct (Product Types) and enum (Sum Types)
  * `impl` are like pointer receivers in Golang
  * `impl` lets struct have methods

```rust
pub struct LList {
    x : i32,
    next: Option<Box<LList >>
}

impl LList {
    fn get(&self) -> i32{
         return self.x;
    }
}


fn main() {
    let mut nodeA = LList{x :8, next: None};
    let nodeB = LList{x: 33, next: None};
    nodeA.next = Some(Box::new(nodeB));
    if let Some(i) = nodeA.next {
        println!("{:?}",i.get());    
    }
    
}

```
