---
title: System Design
tags: tech, cloud
toc: y
---

# Strucure

1. Short summary of service
2. Requirements
    * Function
    * Non-functional
    * Extended
3. Capacity Estimation Constraint
    * Storage Est
    * Bandwidth Est
    * High level Est
4. API's /Client-workflow Diagram/design
5. High level Component Design
    * DB Schema
6. Data Partitioning
7. Cache
8. Load balancing
9. Fault Tolerance and Replication

# Post-Comment-Subcomment
```plantuml
@startuml
entity User {
  * user_id PK
  * name
}

entity Post {
  * post_id PK
  * user_id FK
  * post_content
}
entity Comment {
  * comment_id : PK
  * user_id : FK
  * post_id : FK
  * comment_content
}

User ||--o{ Post
Post ||--o{ Comment
User ||--o{ Comment
Comment ||--o{ Comment
@enduml
```


