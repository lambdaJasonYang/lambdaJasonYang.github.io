---
title: LeanSetup
tags: lean, mathcs, puremath ,categorytheory
toc: y
---

elan is used to install/update lean  

```bash
elan show #shows current lean versions
```

```bash
elan self update
elan default leanprover/lean4:stable #sets lean 4 as default
elan override set leanprover/lean4:stable #set lean 4 for this dir only
```


The lakefile inside somefolder

```{.hs filename=lakefile.lean}
import Lake
open Lake DSL

package Customlib {
  defaultFacet := PackageFacet.oleans
}
```

```{.hs filename=Customlib.lean}
import Customlib.X
import Customlib.Y
```

* customlib
  * lakefile.lean
  * Customlib.lean
  * Customlib   [folder]
    * X.lean
    * Y.lean
* lakefile.lean
* Foo.lean

To use the mycustomlib, in the outer lakefile

```{.hs filename=lakefile.lean}
import Lake
open System Lake DSL

package foo {
  
  dependencies := #[{ name := `customlib, src := Source.path (FilePath.mk "customlib") }]
  }
```