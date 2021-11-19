---
title: Quick Redis
tags: prog, QuickCode
---


Redis uses port 6379

```bash
docker start -p 4000:6379 redis:latest 
redis-cli -h 127.0.0.1 -p 4000
```

Redis is a hashtable {key::TypeX -> value::TypeY}

### Basic data types, Polymorphism - datatypes can change

Basic data type for VALUE  
 
 * String 
 * List   
 * Set  
 * Sorted Set    
 * Hash 



#### Setting a KEY VALUE

```bash
set event:Bleh "heh"
```

* KEY - event:Bleh
* VALUE - "heh" 


##### List

```bash
lpop events
```