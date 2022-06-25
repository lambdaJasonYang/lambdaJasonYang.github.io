---
title: Blockchain
tags: tech, python
---

The main component of the blockchain is the **prevhash** which holds the **history of the chain**.

1. **to build the present you must know your history**
2. **you can only choose one of many possible futures** 

A block chain is just a linked list of blocks which each block holding a history or prevhash.   

`[blockA, blockB, blockC,...]` with blockA as the most recently added aka HEAD.  

We ignore that each block also has hash of itself h(dataA + prevAhash); conceptually not the main point of block chain.  

```text
      +-----------+             +-----------+           +------------+
      |prevAhash= |<--+         |prevBhash= |<-+        |prevChash=  |
      |h(blockB)  |   |         |h(blockC)  |  |        |h(blockD)   |
      +--+--------+   |         +--+--------+  |        +--+---------+
dataA    |            |   dataB    |           |    dataC  |
  |      |            |     |      |           |      |    |
  |      |         hash     |      |        hash      |    |
  |      |            |     |      |           |      |    |
+-v------v-----+      |  +--v------v---+       |  +---v----v---+
|              |      |  |             |       |  |            |
|  blockA      |      +--+   blockB    |       +--+   blockC   |
|              |         |             |          |            |
+--------------+         +-------------+          +------------+
                 -next-->               --next--->
```

```txt
blockA = dataA + prevAhash

prevAhash = h(blockB) = h(dataB + prevBhash)
```

```{.txt filename=singleblock}
     +                  
    / \      =======  blockA
dataA  h(blockB)     

    +
   / \
dataA h   ----------------+
      |                   |
      +                   |
     / \                  | ====== prevAhash = h(blockB)
dataB   h                 | 
        |                 .
        +                 .
       / \                .
  dataC   ..              .
```

* When the head of the tree is a `+` it is a block
* When the head of the tree is a `h` it is a prevhash

Above was a representation of a single block, here we show a blockchain aka a list of the tree above.

```{.txt filename="blockchain"}
           ---next->            ----next->

    +
   / \
dataA h    
      |                   
      +                  +                          
     / \                / \  
dataB   h          dataB   h       
        |                  |
        +                  +                    +
       / \                / \                  / \
  dataC   ..         dataC   ..           dataC   ..

```


---


# General Form



Analogous to [Set theoretical natural numbers](/posts/2018-08-23-BuildNatSets.html)   
  $$Set: N =  Pred(N) \cup \{N-1\} $$
  $$Block: N = d(N) + h(N-1) $$

```txt
n = blockA
d(n) = dataA
h(n-1) = prevAhash

n = d(n) + h(n-1)
```


## Example node

Example BlockA:

* dataA
  * A-text 
  * A-timestamp
  * A-index
  * **A-nonce**
* prevAhash:
  * HASH(dataB)
    * HASH(B-text)
    * HASH(B-timestamp)
    * HASH(B-index)
    * HASH(B-nonce)
  * HASH(prevBhash)

## mining

* **mining** = keep changing the **nonce** until get a good looking hash (There is nothing special or mathematically useful about this constraint or what good looking hash means)  
  * Ex: We want to add a new block, blockA to the chain
  * Continually regenerate blockA with different nonces that gets rejected until we find just the right nonce that gives us a nice looking hash.
  * Even though we need to hash blockA to check if the nonce is nice looking, **we dont add the hash of blockA itself to the chain**.  (We add the unhashed blockA to the chain)
    * When adding new blocks to the chain, hashing the new block is just a temporary artifical method for mining.
* **Confirmation** is when random nodes take the nice nonce and confirm that it is actually nice. Once enough nodes have confirmed the nice nonce, the block will be added to the chain.

![](https://ars.els-cdn.com/content/image/1-s2.0-S240595951930164X-gr1.jpg)

The above mechanism is called Proof-of-Work which stops spamming of artifically generated bad blocks.


# Adversarial Attack

## Making fake transactions

Lets say evil wants to forge a fake transaction.  

* Evil makes a transaction with a fake forged signature that pretends to be you.  
* Everyone can just look at your wallet public key and check if the signature is valid.  

Only a transaction that is signed with your wallet's private key is valid.  

## Doubling transaction and prevhash

>  **you can only choose one of many possible futures** 

* Evil submits 2 transactions at the same time
  1. newblockX + prevhash: Accepted 
  2. newblockY + prevhash: Rejected due to bad prevhash
      * the newblockY needs **newprevhash** = hash(newblockX+prevhash) 
  * **Impossible for both transactions to be added** 
    * if you add newblockX first, and tried to add newblockY, it would require a new prevhash that accounts for the added newblockX. (same argument if you add newblockY first)
* We cant add both but we can still have 2 different chains with the same history floating around.
  * The "true" chain will be the one that has the most confirmations.

This means that you may not know which transaction will pass and which will be rejected if you somehow spend at the same time.

summary:  
**prevhash is a mutating constraint which emulates a lock.**  
**To add a block to the chain, one must fulfil the constraint but once the constraint is fulfilled, it obsoletes all other block additions since a new constraint(newprevhash) must be fulfilled.**


```python
import hashlib
import datetime

def stringify(*args):
    toStr = lambda x: ''.join(list(map(str,x)))
    strData = toStr(list(args))
    return strData

def hashing(stringdata):
    key = hashlib.sha256()
    key.update(stringdata.encode("utf-8"))
    return key.hexdigest()

class Block():
    def __init__(self, index, timestamp, data, prevBlockhash, selfhash):
        self.index = index
        self.timestamp = timestamp
        self.data = data
        self.prevBlockhash = prevBlockhash
        
        self.selfhash = selfhash
    def __str__(self):
        return stringify(self.index,self.timestamp,self.data,self.prevBlockhash)
    

def verify(chain):
    ChainLength = len(chain)
    print(f"{ChainLength} is length of blockchain\n")
    
    def validatePrevBlockhash(k):
        CurrBlockPresentation = chain[k].prevBlockhash
        manualHashPrev = hashing(str(chain[k-1]))
        if manualHashPrev != CurrBlockPresentation:
            print(
    f"""{k}:current-block presents PREVIOUS block hash as: {CurrBlockPresentation} 
manual hashing of the previous block in chain: {manualHashPrev} 
Current-block is malicious and tampered with chain history thus altering the previous block hash\n""")
            return False
        return True
    
    def validateCurrBlockData(k):
        CurrBlockPresentation = chain[k].selfhash
        manualHashCurr = hashing(str(chain[k]))
        if manualHashCurr != CurrBlockPresentation:
            print(
    f"""{k}:current-block presents a hash of: {CurrBlockPresentation}
Manual hashing of current blocks data shows a hash of: {manualHashCurr}
Malicious Current-block is not presenting a valid hash for it's data\n""")
            return False
        return True
                  
    def validateIndexOrdering(k):
        CurrBlockPresentation = chain[k].index
        manualChainIndex = k
        if manualChainIndex != CurrBlockPresentation:
            print(
    f"""{k}:current-block presents index of {CurrBlockPresentation}
Iterating the chain shows an index of {k}
Current-block may have been too slow to add to chain, or a malicious actor is trying to reorder chain\n""")
            return False
        return True
                  
    def validateTimeStampOrdering(k):
        if chain[k-1].timestamp >= chain[k].timestamp:
            print(
    f"""{k}:current-block presents date as {chain[k].timestamp}
previous block was dated {chain[k-1].timestamp}
Currentblock may have been too slow to add to chain, or a malicious actor is trying to reorder chain\n""")
            return False
        return True

    for i in range(ChainLength):
        validateCurrBlockData(i)
        validateIndexOrdering(i)
        if i != 0:
            validatePrevBlockhash(i)
            validateTimeStampOrdering(i)


# dummyblockhash = hashing(str("blah"))
    
mychain = []

FirstBlockData = (0,datetime.datetime.utcnow(),"Genesis","NoPrevBlockHash")
selfhash1st = hashing(stringify(*FirstBlockData))
FirstBlock = Block(*FirstBlockData,selfhash1st)
mychain = [FirstBlock]

SecondBlockData = (1,datetime.datetime.utcnow(),"afsaw",selfhash1st)
selfhash2nd = hashing(stringify(*SecondBlockData))
SecondBlock = Block(*SecondBlockData,selfhash2nd)
mychain = mychain + [SecondBlock]

ThirdBlockData = (2,datetime.datetime.utcnow(),"esfr",selfhash2nd)
selfhash3rd = hashing(stringify(*ThirdBlockData))
ThirdBlock = Block(*ThirdBlockData,selfhash3rd)
mychain = mychain + [ThirdBlock]

BadBlockData = (1,datetime.datetime.utcnow()-datetime.timedelta(seconds=2),"awefaw",selfhash1st)
selfhashBad = hashing(stringify(*BadBlockData))
BadBlock = Block(*BadBlockData,selfhashBad)
mychain = mychain + [BadBlock]


verify(mychain)

# 4 is length of blockchain

# `3:` refers the the 4th block aka BadBlock
# 3:current-block presents index of 1
# Iterating the chain shows an index of 3
# Current-block may have been too slow to add to chain, or a malicious actor is trying to reorder chain

# 3:current-block presents PREVIOUS block hash as: 4feac5456f60ae87430efc3aa70be96ad7285a897e5c2fc36240bb391ebe04bd 
# manual hashing of the previous block in chain: 2ff87d74f8f4ee982a034b916d1246364a95b6078817bcc41aa2a43f4f2b8943 
# Current-block is malicious and tampered with chain history thus altering the previous block hash

# 3:current-block presents date as 2022-04-21 05:13:07.327116
# previous block was dated 2022-04-21 05:13:09.326804
# Currentblock may have been too slow to add to chain, or a malicious actor is trying to reorder chain
```


# Aside

Hashing data just means:  

1. convert data to string
2. append string together
3. hash this new string

```python
import hashlib
import datetime

TESTindex = 0 
TESTtimestamp = datetime.datetime.utcnow()
TESTdata = 'Genesis'
TESTprevious_hash = 'arbitrary'

TESTkeyA = hashlib.sha256()
TESTkeyB = hashlib.sha256()
encodeUTF8 = lambda x: str(x).encode("utf-8")
toStr = lambda x: ''.join(list(map(str,x)))

prehashA = toStr([TESTindex,TESTtimestamp,TESTdata,TESTprevious_hash])
TESTkeyA.update(prehashA)

TESTkeyB.update(encodeUTF8(TESTindex))
TESTkeyB.update(encodeUTF8(TESTtimestamp))
TESTkeyB.update(encodeUTF8(TESTdata))
TESTkeyB.update(encodeUTF8(TESTprevious_hash))

assert TESTkeyA.hexdigest() == TESTkeyB.hexdigest()

print(prehashA)
#'02022-04-21 03:22:57.863939Genesisarbitrary'
print(TESTkeyA)
#'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
print(TESTkeyB)
#'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
```
