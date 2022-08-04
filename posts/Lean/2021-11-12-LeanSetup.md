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


# Installing git dependencies like mathlib

```{.hs}
import Lake
open Lake DSL

require mathlib from git 
  "https://github.com/leanprover-community/mathlib4"

package helloWorld {
  -- add configuration options here
}

```


```{.hs}
def fileTargetWithDepList (file : FilePath) (depTargets : List (BuildTarget i))
(build : List i → BuildM PUnit) (extraDepTrace : BuildM _ := pure BuildTrace.nil) : FileTarget :=
  fileTargetWithDep file (Target.collectList depTargets) build extraDepTrace

```

# FFI

```{.hs}
import Lake
open System Lake DSL

--fileTargetWithDep : {i : Type} → FilePath → BuildTarget i → (i → BuildM PUnit) → optParam (BuildM BuildTrace) (pure BuildTrace.nil) → FileTarget
/-
{i : Type} is FilePath:Type
FilePath is `("./build/myfuns.o")`
BuildTarget FilePath is `("./myfuns.cpp")`
(FilePath → BuildM PUnit) is  `(λ srcFile => do compileO ...`
-/
/-
compileO : FilePath → FilePath → optParam (Array String) #[] → optParam FilePath { toString := "cc" } → BuildM PUnit
FilePath is `("./build/myfuns.o")`

-/

def ffIOTarget : FileTarget := 
  fileTargetWithDep ("./build/myfuns.o") ("./myfuns.cpp") (λ srcFile => do
    compileO ("./build/myfuns.o") srcFile #["-I", (← getLeanIncludeDir).toString, "-fPIC"] "c++")
--`(← getLeanIncludeDir).toString` is "/home/USERNAME/.elan/toolchains/leanprover--lean4---nightly-2022-07-29/include"


--staticLibTarget : FilePath → Array FileTarget → optParam (Option FilePath) none → FileTarget
/-
FilePath is `"./build/leanffi.a"`
Array FileTarget is `#[ffIOTarget]`
-/

extern_lib cLib := ffIOTarget
  /-staticLibTarget ("./build" / nameToStaticLib "leanffi") #[ffIOTarget]
-- `nameToStaticLib "leanffi"` is "leanffi.a"
-/


--Below is not relevant for FFI

--script doesnt run on lakebuild. Use script to investigate variables.
--script can be ran with `lake script run logVar`
script logVar do
  IO.println __dir__ 
  IO.println defaultBuildDir
  IO.println (← getLeanIncludeDir).toString 
  IO.println (nameToStaticLib "leanffi")
  return 0



package t1   {
  --srcDir is dir where the entrypoint Main.lean is located
  srcDir := "." --entry points to ./srcDir/./Main.lean
 
}

```