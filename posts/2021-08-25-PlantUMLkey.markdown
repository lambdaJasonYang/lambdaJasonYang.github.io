---
title: PlantUML diagram list
tags: musings, prog
---

```plantuml
@startuml
abstract        abstract
abstract class  "abstract class"
annotation      annotation
()              circle
class           class
<>              diamond
entity          entity
enum            enum
interface       interface
@enduml
```

---


### Sequence diagram
```plantuml
@startuml
participant User

User -> A: DoWork
activate A

A -> A:  Call Self

A -> B: << createRequest >>
activate B

B -> C: DoWork
activate C
C --> B: WorkDone
destroy C

B --> A: RequestCreated
deactivate B

A -> User: Done
deactivate A

@enduml
```

* Step into function call
  * `->`{.code}
* Step out/return from call
  * `-->`{.code}


```plantuml
@startuml
activate B
B -> B:
activate B


B -> B:
activate B

B --> B:
deactivate B

B --> B:
deactivate B
@enduml
```

```plantuml
@startuml
autoactivate on
B -> B:


B -> B:
B -> B:

return
return

@enduml
```