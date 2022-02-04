---
title: Software Engineering lessons
tags: OOP, prog
toc: y
---


# On JS and Golang

## Closures and Objects

> Closures are the Poor man's Objects

* Closures means that our function remembers it's state through it's surrounding environment
* JS functions are Objects
* Golang functions can also behave like Objects using closures

```go
func adder() func(int) int {
	sum := 0
	return func(x int) int {
		sum += x
		return sum
	}
}

func main() {
	pos, neg := adder(), adder()
	for i := 0; i < 10; i++ {
		fmt.Println(
			pos(i),
			neg(-2*i),
		)
	}
}
```

## Promises

* JS promises are just continuations
* JS async await is just a callcc monad

[](http://webcache.googleusercontent.com/search?q=cache%3Ahttps%3A%2F%2Fgist.github.com%2FMaiaVictor%2Fbc0c02b6d1fbc7e3dbae838fb1376c80&rlz=1CAUSZT_enUS979US979&oq=cache%3Ahttps%3A%2F%2Fgist.github.com%2FMaiaVictor%2Fbc0c02b6d1fbc7e3dbae838fb1376c80&aqs=chrome..69i57j69i58.3079j0j4&sourceid=chrome&ie=UTF-8)

# Interface vs Abstract Class

Abstract Class is just interfaces with a base(Default) class implementation.  
Having both Interface and Abstract Classes is redundent.

Just use Interfaces.

* Problem: Adding new parameters to an interface can break all implementations
* Solution: Create a new interface with the new parameter that inherits the old interface.
  * The new interface is a superset of the old interface.


# Polymorphism, Dynamic Dispatch, Prototypes

* Dynamic Dispatch: When class inherits a chain of parent classes, when a method is called but not found, it will keep moving up the chain of parents until it finds a method or return an error.
  * This is the main feature of polymorphism
    * growl() method of parent class Animal called differently(Dynamically Dispatched) by the cat and dog class that inherits growl().

<!-- -->
* Dynamic Dispatch is also called Delegation
* In static languages, it's called Static Dispatch
<!--  -->
* JS `._proto_` is given too all JS objects.

# Workflow

1. Jira, recieve task, remove completed tasks
2. Slack, collab
3. Documentation, markdown Slab, Confluence, Azure DevOps wiki
4. Version control git
5. Code management,releases and versions workflow, Github, Azure DevOps
6. Coding
7. Learn company framework
8. Use testing framework , mocking , text fixture management, code coverage report
9. testing to CI pipeline - takes code and runs test 
10. linting code
11. dependency management
12. Submit PR, after acceptance code goes though CD 
13. SQL 

## Terms

* API, IOT, IaaS SaaS PaaS DaaS MSaaS MBaaS DCaaS iPaaS ITMaaS , CRM, MIS, ERP, HRM, CM,GIS, DBMS, CAD