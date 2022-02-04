---
title: Quick Setup
tags: prog, C, QuickCode, OS
---

# Haskell

```plantuml
	
@startsalt
{
{T
 + helloworld
 ++ app
 +++ Main.hs
 ++ src
 +++ Lib.hs
 ++ test
 +++ Spec.hs
 ++ helloworld.cabal
 ++ package.yaml
 ++ stack.yaml
}
}
@endsalt
```

Creates a new folder helloworld at current directory
```bash
stack new helloworld new-template
```

## running
```bash
stack run ./app/Main.hs
```
##### running interactive
runs an interactive env
```bash
stack ghci ./app/Main.hs
```

##### build+execute
```bash
stack build
stack exec hello-world
```

---

### Go

```{.go filename="hello.go"}
package main

import (
	"fmt"
)

func main() {
	var a string = "hello"
	var bb []byte = []byte(a[0:3])
	var c string = a[1:3]

	fmt.Printf("%s\n", a)
	fmt.Printf("%s\n", string(bb))
	fmt.Printf("%s\n", c)
}

```

```bash
go run hello.go
```

### Go Modules

```plantuml
	
@startsalt
{
{T
 + goworld
 ++ greetings
 +++ go.mod
 +++ greetings.go
 ++ hello
 +++ go.mod
 +++ hello.go
}
}
@endsalt
```


1.  ##### project folder
```bash
mkdir goworld
cd goworld
```

2. ##### Create greetings modules
```bash
mkdir greetings
cd greetings
go mod init example.com/greetings
#this creates go.mod
```

create a file called greetings.go
```{.go filename=greetings.go}
package greetings

import "fmt"

// Hello returns a greeting for the named person.
func Hello(name string) string {
    // Return a greeting that embeds the name in a message.
    message := fmt.Sprintf("Hi, %v. Welcome!", name)
    return message
}
```

3. ##### executable folder
```bash
cd ..
mkdir hello
cd hello
go mod init example.com/hello
```

```{.go filename=hello.go}
package main

import (
    "fmt"

    "example.com/greetings"
)

func main() {
    // Get a greeting message and print it.
    message := greetings.Hello("Gladys")
    fmt.Println(message)
}
```

4. ##### setup local
typically our modules will be held on our webserver at (in this case) "example.com/greetings"  
To run locally   

in your hello dir  
```bash
go mod edit -replace example.com/greetings=../greetings
go mod tidy
```

5. ##### running

inside hello dir
```bash
go run .
```


---

### C


**running**
```bash
gcc -o bleh main.c
./bleh
```