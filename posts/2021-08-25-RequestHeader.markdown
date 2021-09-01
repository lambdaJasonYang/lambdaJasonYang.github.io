---
title: HTTP requests - Design proper headers in python
tags: mathcs, finance, python
---

In python, headers are key-value pairs in dicts.

Some api's require that headers must satisfy certain requirements:
* "User-Agent": "Mozilla/5.0 (Windows NT 10.0)"
 * messing with User-Agent; even using  a valid one like "Mozilla/5.0 (X11; U; Linux i686)" can make your request hang or timeout. 
*  "sec-fetch-mode": "cors"



adding "accept-encoding": "gzip, deflate, br", will print out hex so leave it out of your header if you can

```python
import urllib.request
url = "https://api.example.com/api/endpoint"

GOODheader = {
"Content-Type": "application/json",
'Accept': 'application/json, text/plain, */*',
                 'DNT': "1",
                 'Origin': 'https://www.example.com/',
                 'Sec-Fetch-Mode': 'cors',
                 'User-Agent': 'Mozilla/5.0 (Windows NT 10.0)'
    }
req = urllib.request.Request(url,headers=GOODheader,method="GET")
page = urllib.request.urlopen(req,timeout=5)
print(page.read(300))
```

Here is another sample request header

```python
SAMPLEheaderWithEncoding = {

"Accept": "text/html,application/xhtml+xml, text/plain,application/xml;q=0.9,*/*;q=0.8",
"scheme":"https",
"Referrer Policy": "strict-origin-when-cross-origin",
"accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
"accept-encoding": "gzip, deflate, br",
"accept-language": "en-US,en;q=0.9",
"cache-control": "max-age=0",
"DNT": "1",
"sec-ch-ua": "' Not A;Brand';v='99', 'Chromium';v='92'",
"sec-ch-ua-mobile": "?0",
"sec-fetch-dest": "document",
"sec-fetch-mode": "cors",
"sec-fetch-site": "none",
"sec-fetch-user": "?1",
"upgrade-insecure-requests": "1",
"Content-Type": "application/json",
'Origin': 'https://www.example.com/',
"user-agent": "Mozilla/5.0 (Windows NT 10.0)"
            }
```