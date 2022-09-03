---
title: Quick Springboot
tags: prog, QuickCode
---

# VScode commands

* New Class
  * Ctrl+Shift+P `>Java: New Java Class`
* Adding spring plugins after creating spring project.
  * Ctrl+Shift+P `>Spring Initializr: Add Starters...`
* Adding Class Constructors, getters/setters
  * Right-Click > "Generate Constructors"
  * Right-Click > "Generate Getters and Setters"


# Vanilla Project

* mvnw
* pom.xml
* src/
  * main /
    * java/
      * com.example.demo
        * DemoApplication.java
    * resources/
      * static/
      * templates/
      * application.properties
    * test/
      * java/
        * com.example.demo
          * DemoApplicationTests.java

```{.java filename="DemoApplication.java"}
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class DemoApplication {

	public static void main(String[] args) {
		SpringApplication.run(DemoApplication.class, args);
	}

}
```



# Demo Project

## Entry Point

* `@Component` annotation tells the class that it is used for DI injection. Note annotation is completely diff from decorator in python.
  * by default the class is a auto-initialized singleton, meaning even if we do not `Alien a = context.getBean(Alien.class);` the Alien class will automatically get constructed and call it's constructor `"Alien constructed"` only ONCE
  * `@Scope(value="prototype")` annotation means the class is not an auto-initialized singleton, and constructor behaves more like a regular object.

```{.java group="a1" glabel="main"}
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
public class DemoApplication {

	public static void main(String[] args) {
		ConfigurableApplicationContext context = SpringApplication.run(DemoApplication.class, args);
		//Alien a = context.getBean(Alien.class);
		
	}

}
```

```{.java group="a1" glabel="singleton"}
import org.springframework.stereotype.Component;

import org.springframework.stereotype.Component;

@Component
public class Alien {
    private int aid;
    private String aname;
    public Alien() {
        super();
        System.out.println("Alien constructed")
    }
    public int getAid() {
        return aid;
    }
```

```{.java group="a1" glabel="prototype"}
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(value="prototype")
public class Alien {
    private int aid;
    private String aname;
    public Alien() {
        super();
        System.out.println("Alien constructed")
    }
```

# IOC

Bean is just a java class with private variable, getters and setters

* `ConfigurableApplicationContext context` is the IOC container. 
  * `context.getBean(Alien.class);` indicates that context can build the Alien bean meaning context is a bean factory.

# Proxy pattern

* A proxy wraps around all objects\