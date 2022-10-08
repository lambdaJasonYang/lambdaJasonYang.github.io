---
title: Design Patterns
tags: OOP, prog
toc: y
---

# Theory

```txt

Person --> [API/Controller] --> [Service Layer] --> [Data Access Layer] --> [SQL DB]

```



* Model holds the class 
  * We do a bijective map from Model to DB using **spring JPA annotation** which means the SQL db query to create the associated Model class will be autogenerated
* Controller holds the routes but it calls the functions by instantiating then using the Services (alternatively dependency injection of services)
* Services is just a library of functions but we just put these function in some class called "blehServices"
  * Since the functions in "blehServices" is stateless, it is easy to pack it for dependency injection which creates a blehServices singleton and injects it into the Controller that needs it.
* Data access layer is an interface repository pattern that uses **spring JPA** 

# Spring patterns

* IOC - Spring's Application context is the IOC. Increases testability, Decrease coupling and enforces coding to interfaces.
* Proxy - Every object in Spring is wrapped in one or more Proxy. Allow us to give additional functionality.
* Factory - IoC container in the startup is a factory
* Singleton + Prototype - Most Spring bean classes use one of these two patterns.  
Bean classes default to Singleton but with annotations can be Prototype.
* Template - Used in remote calls. Most common are JDBC and REST  
Third parties use the Template pattern to plugin easily to Spring
* MVC - Webpages and RESTFUL services use MVC

## IOC

* IOC is the one that constructs and maintains the reference of objects



# General

## Flyweight

* Java String uses flyweight
* Basically a hashmap of singletons
* Reduces memory consumed by limiting repetitive objects



# API

* @RequestParam ← application/x-www-form-urlencoded
* @RequestBody ← application/json,
* @RequestPart ← multipart/form-data,