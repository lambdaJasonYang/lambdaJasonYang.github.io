---
title: Thinkscript
tags: musings, misc
toc: y
---

# code

* `def`{.bash} to instantiate variables
* Booleans are 1 or 0 , "yes" or "no" 


main default variables:

```bash
def a = open + high + low + close 
```
default variables have implicit index of current day $n$  

`open`{.bash} := $Open(n)$


## Index

Index is relative to the current day $n$ in the negative direction.    
  
`volume[1]`{.bash} := $Volume(n-1)$

```bash
def diff = close - close[1]
#close - previous day close
```

## Recursive

```bash
def data = data[1] + volume;
plot stuff = data;
```

$$ F_n = F_{n-1} + P(n) $$

* data := $F_n$
* data[1] := $F_{n-1}$
* volume := $P(n)$
* IH is cumulative volume at some arbitrary time

ToS assumes base case $F_0=0$

plot $n \mapsto F_n$  

The above will plot cumulative Volume over time. 

## Flow control

```bash
if 3 > 2 {
  def a = 44 
}
```

## plotting lower subgraph

```bash
declare lower;
c = 5;
plot outX = c;
```

## allow UI dropdown input

```bash
input a = close; #default value is close
plot outX = a;
```

# TDAPI

CONSUMER KEY is XXXXXXXXXXXXXXXXXXX  
The only thing permanent is the CONSUMER KEY.

## Price access 

We have no need to get a refresh token and access token if we just need price.  
Just use CONSUMER KEY.

https://api.tdameritrade.com/v1/marketdata/AAPL/pricehistory?apikey=XXXXXXXXXXXXXXXXXXX&periodType=day

## Account access


* PERMANENT: CONSUMER KEY 
  * ONE-OFF: GET CODE FROM URL FROM URL LOGIN, LOCALHOST 
    * USE ONE-OFF CODE TO GET 90 day REFRESH-TOKEN
      * USE 90 day REFRESH TOKEN to GET 30 min ACCESS TOKEN 

This means every 90 days we have to login using our browser to get a one-off code through localhost url for a new refresh token. Every 15 min if use the api we need to use this 90-day refresh token to get an access token.

1. Register an App with callback url `http://localhost`
2. Get the CONSUMER KEY `XXXXXXXXXXXX`
3. Go to URL `https://auth.tdameritrade.com/auth?response_type=code&redirect_uri=http%3A%2F%2Flocalhost&client_id=XXXXXXXXXXXX%40AMER.OAUTHAP`
4. This will open an offical TDAmeritrade page you have to login
5. Redirects you to `http://localhost/?code=Z%21ZZZZZ%23Z%25ZZZZ%21Z`
6. get the string after `http://localhost/?code=` which is `Z%21ZZZZZ%23Z%25ZZZZ%21Z`
7. decode urlencoding using python

```python
#url decoder
from urllib.parse import unquote
urlcode = "Z%21ZZZZZ%23Z%25ZZZZ%21Z"
code = unquote(urlcode)
print(code)
#output: Z!ZZZZZ#Z%ZZZZ!Z
```

8. go to [https://developer.tdameritrade.com/authentication/apis/post/token-0](https://developer.tdameritrade.com/authentication/apis/post/token-0)
9. 

```bash
grant_type: authorization_code  (literally type "authorization_code" into the box without quotes)
refresh_token: (leave blank)
acccess_type: offline (literally type "offline" into the box without quotes)
code: Z!ZZZZZ#Z%ZZZZ!Z (output from our python urldecode)
client_id: CONSUMERKEY@AMER.OAUTHAP (yes, add "@AMER.OAUTHAP" without quotes)
redirect_uri: http://localhost
```

10. 



```bash
{
  "access_token": "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
  "refresh_token": "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
  "scope": "PlaceTrades AccountAccess MoveMoney",
  "expires_in": 1800,
  "refresh_token_expires_in": 7776000,
  "token_type": "Bearer"
}
```