---
title: Python Concurrency
tags: python, prog, concurrency
---

* IO-bound , Slow IO, Many Connections  
  * Use Async
  * example: web client requests/downloads(IO) 

<!--  -->

* Content from Server IO-bound , Fast IO , Limited Connections    
  * Use Multi-thread 
* Non IO-bound, CPU calculations 
  * Use Multi-processing


~~hello~~

# Threading

``` python
import threading
def fstThread(): #parent thread
    sndThread = threading.Thread(target=someFunction,args=(2,))
    sndThread.start()
    someFunction(1)
    
def someFunction(k): #child thread
    print(f"{k} from {threading.current_thread().name}\n")
    
fstThread()
#>2 from Thread-6
#>1 from MainThread
```

# Multiprocessing

```python
import multiprocessing
def fstProcess():
    sndProcess = multiprocessing.Process(target=someFunctionA,args=(2,))
    sndProcess.start()
    someFunctionA(1)
def someFunctionA(k):
    print(f"{k} from {multiprocessing.current_process().name}\n")
    
fstProcess()
##Note using IDLE or Jupyter, the Child process shows no output 
##Using python script the output would look similar to the threading output, 2 prints
```

#### Race condition

```python
import threading
from time import sleep

count = [0]

def inc():
    temp = count[0]
    sleep(0)
    count[0] = temp + 1

other = threading.Thread(target = inc, args = ())
other.start()
inc()
print(f"count is {count[0]}") #output is sometimes 2, sometimes 1
```

#### Producer Consumer non-synchronized

```python
import threading
from time import sleep

Items = []
Commands = []

def consume():
    while len(Commands) == 0:
        pass #stuck in this loop waiting for Items.append loop to end
    print(f"item is {Items}")

def produce():
    consumer = threading.Thread(target=consume,args=())
    consumer.start()
    for i in range(10):
        Items.append(i)
    Commands.append("go")
produce()
```

#### Producer Consumer synchronized

```python
from queue import Queue

queue = Queue()

def synchronized_consume():
    while True:
        print('got an item:', queue.get())
        queue.task_done() #

def synchronized_produce():
    consumer = threading.Thread(target=synchronized_consume, args=())
    consumer.daemon = True #Parent or "synchronized_produce()" process will not wait for the consumer before exiting
    consumer.start()
    for i in range(10):
        queue.put(i)
    queue.join()

synchronized_produce()
```

#### Lock

```python
seen = set()  ##The set that we will sychronize with a lock
seen_lock = threading.Lock()

def already_seen(item):
    with seen_lock:
        if item not in seen:  # Only one thread is checking if item is in sychronizd set
            seen.add(item)   #modifying a sychronized set, means only one thread can add to the set seen
            return False
        return True
```

#### Barrier

```python
counters = [0, 0]
barrier = threading.Barrier(2)

def count(thread_num, steps):
    for i in range(steps):
        other = counters[1 - thread_num]
        barrier.wait() # wait for reads to complete
        counters[thread_num] = other + 1
        barrier.wait() # wait for writes to complete

def threaded_count(steps):
    other = threading.Thread(target=count, args=(1, steps))
    other.start()
    count(0, steps)
    print('counters:', counters)

threaded_count(10)
```

#### Message passing

```python
def process_consume(in_pipe):
    while True:
        item = in_pipe.recv()
        if item is None:
            return
        print('got an item:', item)

def process_produce():
    pipe = multiprocessing.Pipe(False)
    consumer = multiprocessing.Process(target=process_consume, args=(pipe[0],))
    consumer.start()
    for i in range(10):
        pipe[1].send(i)
    pipe[1].send(None) # done signal

process_produce()
```