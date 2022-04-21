---
title: Design Patterns
tags: OOP, prog
toc: y
---

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

