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

$$gym \overset{depends}{\rightarrow} gym_yesterday \tag{Red Herring}$$ 
$$Optimal(gym) \overset{depends}{\rightarrow} (Optimal(code_yesterday),Optimal(rest_yesterday)) \tag{Correct}$$

**Think about constraint WRT TO OPTIMALITY, not the action itself**

# Template

```{.py group=A1 glabel=hide}
```
```{.py group=A1 glabel=py}
```


# LC 

## Decoding

Question, How many ways we can decode the string with each word being either 1 or 2 letters.  

* Lesson: How many ways = COUNT # paths in DFS from root to leaf. We can always cache memoize DFS.  


```{.py group=LC91 glabel=hide}
```
```{.py group=LC91 glabel=pyTLE}
"""
either terminate 1-next or terminate 2-next
(1|13..) or (11|3..)
ConstraintA: Reject 2-next when > "26"
ConstraintB: When 2-next is "0", must ONLY terminate 2-next NOT 
(1|03) BAD
(10|3) GOOD

DFS, pruning bad decisions

2 possible cases

(..10) 
(..04) if there is a 0 we must take it
"""
class Solution(object):
    def numDecodings(self, s):
        """
        :type s: str
        :rtype: int
        """
        
        def dfs(s):
            if len(s) == 0:
                return 1
            if len(s) == 1:
                if s == "0":
                    return 0
                return 1
            take1 = s[:1]
            rest1 = s[1:]
            take2 = s[:2]
            rest2 = s[2:]
            if take2[0] == "0":
                return 0
            if int(take2) > 26:
                return dfs(rest1) + 0
            if take1 == "0":
                return 0
            else:
                return dfs(rest1) + dfs(rest2)

        return dfs(s)
        #TLE but correct
```

```{.py group=LC91 glabel=pyMemo}

"""
either terminate 1-next or terminate 2-next
(1|13..) or (11|3..)
ConstraintA: Reject 2-next when > "26"
ConstraintB: When 2-next is "0", must ONLY terminate 2-next NOT 
(1|03) BAD
(10|3) GOOD
ConstraintC: Reject 1-next when "0". This is no map from a single "0".

DFS, pruning bad decisions

2 possible cases

(..10) 
(..04) if there is a 0 we must take it
"""
class Solution(object):
    def numDecodings(self, s):
        """
        :type s: str
        :rtype: int
        """
        def cache(func):
            cache_ = {}
            def wrapper(*args):
                try:
                    return cache_[args]
                except:
                    cache_[args] = func(*args)
                    return cache_[args]
            return wrapper
        @cache      
        def dfs(s):
            if len(s) == 0:
                return 1
            if len(s) == 1:
                if s == "0":
                    return 0
                return 1
            take1 = s[:1]
            rest1 = s[1:]
            take2 = s[:2]
            rest2 = s[2:]
            if take2[0] == "0":
                return 0
            if int(take2) > 26:
                return dfs(rest1) + 0
            if take1 == "0":
                return 0
            else:
                return dfs(rest1) + dfs(rest2)

        return dfs(s)
        #SUCCESS due to cache memo
```

```{.py group=LC91 glabel=pyMemoMan}

class Solution(object):
    def numDecodings(self, s):
        """
        :type s: str
        :rtype: int
        """
        memo = {}
        def dfs(s):
            if len(s) == 0:
                return 1
            if len(s) == 1:
                if s == "0":
                    return 0
                return 1
            take1 = s[:1]
            rest1 = s[1:]
            take2 = s[:2]
            rest2 = s[2:]
            if take2[0] == "0":
                return 0
            if int(take2) > 26:
                dfsOUT1 = -999
                if rest1 in memo:
                    dfsOUT1= memo[rest1] + 0
                else:
                    memo[rest1] = dfs(rest1)
                    dfsOUT1 = memo[rest1] + 0
                return dfsOUT1
            if take1 == "0":
                return 0
            else:
                dfsOUT1 = -999
                dfsOUT2 = -999
                if rest1 in memo:
                    dfsOUT1 = memo[rest1]
                else:
                    memo[rest1] = dfs(rest1)
                    dfsOUT1 = memo[rest1]
                if rest2 in memo:
                    dfsOUT2 = memo[rest2]
                else:
                    memo[rest2] = dfs(rest2)
                    dfsOUT2 = memo[rest2]
                return dfsOUT1 + dfsOUT2
                    
                #return dfs(rest1) + dfs(rest2)

        return dfs(s)
        #Even faster than decorator memo for somereason
```

```cpp
using std::string;
string tail(string mystr){
    return mystr.substr(1);
}

int minDistanceR(string word1, string word2){
    if(word2.size() == 0){
        return word1.size();
    }
    if(word1.size() == 0){
        return word2.size();
    }
    string tail1 = tail(word1);
    string tail2 = tail(word2);
    string head1 = string(1,word1[0]);
    string head2 = string(1,word2[0]);
    if (head1 == head2){
        int cost = 0;
        int opt_Match = cost + minDistanceR(tail1,tail2); 
        return opt_Match;
    }
    if (head1 != head2){
        int cost = 1;
        int opt_Insert = cost + minDistanceR(word1,tail2);
        int opt_Del = cost + minDistanceR(tail1,word2);
        int opt_Subst = cost + minDistanceR(tail1,tail2);
        return std::min(opt_Subst,std::min(opt_Insert,opt_Del));

    }
    return 0;
}
```

# UVA

## 495 fib

* To return arrays in C++, you must return pointer
* all function local var are destroyed in C++ unlike python or JS. 
  *  This means you cant initialize an array in a function and hope to return it for use in main

```cpp
int* fib(int dp[]){
    dp[0] = 1;
    dp[1] = 1;
    for(int i = 2; i < 5001; i++){
        dp[i] = dp[i-1]+dp[i-2];
    }
    return dp;
}

using std::cin;

int main(){
    int dp[5001];
    fib(dp);
    print(dp[4]);
    int n;
    while(cin >> n){
        print(dp[n]);
    }

}
```