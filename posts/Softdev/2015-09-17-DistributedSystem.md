---
title: Distributed Systems, CAP, Raft
tags: tech, cloud
toc: y
---

Issues:

* bad performance in parallel algorithms due to load imbalance 
* deadlocks 

```plantuml
@startmindmap

* Distributed\nSystem
 * Shared-Memory model

left side

 * Message-Passing model
@endmindmap

```

# RPC

```{.js filename="Client"}
Result result = paymentService.processPayment("192831823",4.99,"USD")
```

* Example: paying for an item on a webpage and sending the payment request to a credit card server
* **Stub** 


# Raft and Paxos

* Raft is a better Paxos
* Each node of the distributed system has 1 of 3 states:
  * Follower : All Nodes begin in this state
    * Followers that dont hear from Leaders become Candidates
  * Candidate : Request votes from other nodes and hear their reply
    * Leader Election: Candidate becomes leader if it hears the vote from majority of nodes
  * Leader : All changes in the system goes through the leader

# CAP and distributed Databases

## Distributed DB
* we can think of distributed DBs as read-write storage with just 2 operations `get()` `set(X)` on a single register but operations are labeled by clients A,B,... `A:get(), B:set(10) A:set(10)` 
* `A:get(), B:set(10) A:set(10)` means client A sent get request to the distributed DB and client B sent a set request to the distributed DB but we don't know who sent it first. But we do know `A:get` is before `A:set(10)`.

## CAP 
* CAP is not a choice of CA,CP,AP ... 
* P aka Partition is not a choice, it is an act of nature aka a unforeseen crisis
* CAP really means **If P happens we can choose to save either C or A but not both**.
  * Paxos/Spinnaker/Raft choose C

## Linearisable Consistency aka Atomic

* C in CAP means Strong Consistency aka Linearisable Consistency aka Atomic
* Atomic: Everything appears to be a sequential serial operation on the register

* eventual consistency: `set(10), set(5), get()=10`
  * Eventual Consistency mutually exclusive to Atomicity

## Availability 

* Availability means all request to DB must return non-error requests is a finite amount of time
* Async means no bound on receiving the response of a message, it may never return

