---
title: CF Problem Sets

tags: mathcs, algorithms
---

# 698A - DP

```{.py group=A1 glabel=hide}
```
```{.py group=A1 glabel=py}
nil = input()
a = [int(x) for x in input().split()]
n = len(a)

dp = [[999999 for x in range(0,3)] for y in range(0,n)]

REST = 0
GYM = 1
CODE = 2

NO_CODEGYM = 0
NO_GYM = 1
NO_CODE = 2
YES_BOTH = 3

def basecase():
    dp[0][REST] = 1
    if a[0] == NO_GYM or a[0] == YES_BOTH:
        dp[0][CODE] = 0
    if a[0] == NO_CODE or a[0] == YES_BOTH:
        dp[0][GYM] = 0

def restPrev(i):
    restVal =dp[i-1][REST]
    gymVal = dp[i-1][GYM]
    codeVal = dp[i-1][CODE]
    return 1 + min([restVal,gymVal,codeVal]) 
def codePrev(i):
    restVal =dp[i-1][REST]
    codeVal = dp[i-1][CODE]
    return min([restVal,codeVal])
def gymPrev(i):
    restVal =dp[i-1][REST]
    gymVal = dp[i-1][GYM]
    return min([restVal,gymVal])

def lastday(i):
    restVal =dp[i-1][REST]
    gymVal = dp[i-1][GYM]
    codeVal = dp[i-1][CODE]
    return min([restVal,gymVal,codeVal])

basecase()
# print(dp)
for k in range(1,n):
    #print(dp[k])
    if a[k] == NO_CODEGYM:
        dp[k][REST] = restPrev(k)
    if a[k] == NO_CODE:
        dp[k][REST] = restPrev(k)#optimal(rest) today is based on optimal(rest) yesterday
        dp[k][GYM] = codePrev(k)#optimal(gym) today is based on optimal(code) yesterday
    if a[k] == NO_GYM:
        dp[k][REST] = restPrev(k)#optimal(rest) today is based on optimal(rest) yesterday
        dp[k][CODE] = gymPrev(k)#optimal(code) today is based on optimal(gym) yesterday
    if a[k] == YES_BOTH:
        dp[k][REST] = restPrev(k)
        dp[k][CODE] = gymPrev(k)
        dp[k][GYM] = codePrev(k)
        

print(lastday(n))

```

## Takeaway

* Initialize all value in dp as INF or unoptimal
* look at the code, how does it account for rejecting 2 consecutive days?
  * Notice that (gym) --depends--> (gym yesterday)
  BUT Optimal(gym) --depends--> (Optimal(rest) yesterday,Optimal(code) yesterday)

$$gym \overset{\rightarrow}{depends} gym_yesterday \tag{Red Herring}$$ 
$$Optimal(gym) \overset{\rightarrow}{depends} (Optimal(code_yesterday),Optimal(rest_yesterday)) \tag{Correct}$$

**Think about constraint WRT TO OPTIMALITY, not the action itself**

# Template

```{.py group=A1 glabel=hide}
```
```{.py group=A1 glabel=py}
```