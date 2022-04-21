---
title: Quick Springboot 1 terms
tags: prog, bean, java, QuickCode
---

# Basics

## Terms

* POJOS - Plain Old Java Objects, Any class with attributes
* Java Beans - Simple Objects(POJOS) with only getters and setters. Example of encapsulation
* Spring Beans - POJOS that are configured in Spring's `ApplicationContext`
* DTO - Data Transfer Object, are Java Beans that transfer state
* `ApplicationContext` wraps the **Bean Factory** which injects Spring Beans at runtime.  
  * **Bean Factory is the IOC Container**
  * Bean Factory maintains references of the Bean it creates

: Tomcat - The web server also a servlet container
: Servlets are classes that respond/reply to REST. 

* JPA - Java Persistence API, Hibernate ORM

## Service abstraction

example: we separate view layer, from stateless business logic layer and stateful DB layer.  

1. Define the interface
2. Create API for the interface
3. Inject the dependencies
4. Annotate or Config to customize when it is injected
5. 


## Adding dependencies

```xml
		<dependency>
			<groupId>org.springframework.boot</groupId>
			<artifactId>
				spring-boot-starter-data-jpa
			</artifactId>
		</dependency>
```

* spring-boot-starter-data-jpa : Prereq for DB operation
* com.h2database : In memory db


# Refactoring

Moving package "data" into package "com.example.demoSpring"

Right-click package >> Refactor >> Rename >> "com.example.demoSpring.data" + check "Update references" + "Rename subpackages"

