---
title: Transfer files SFTP
tags: tech
---


### On remote target linux

```bash
service ssh start
```

### On local windows

```bash
sftp kali@192.168.1.184
cd Desktop #this puts us in the remote ~/Desktop
ls 
mkdir transferredFiles #makes new dir in remote ~/Desktop
cd transferredFiles
put -r C:\Users\User\Desktop\someFolder 
bye
```
transfers file from Windows to Linux, C:\Users\User\Desktop\someFolder to ~/Desktop/transferredFiles


To transfer file from Linux to Windows, ~/kali/someFolder to C:\Users\User\Desktop

instead of `put -r C:\Users\User\Desktop\someFolder `{.bash}  
use `get -r ~/kali/someFolder C:\Users\User\Desktop`{.bash}


# Reading base64 encoded files

```bash
curl "https://..." | python this.py
```

```py
#this.py
import base64
import codecs
import json
import sys

presentraw = []
for line in sys.stdin:  #reads bash input from curl
    presentraw = line 

#reads base64 encoded example file
with codecs.open('example','r',encoding="base64") as f:
    dfile = f.read()
    past = json.loads(dfile)['candles']
    recent = -1
    

```

# Convert list of dicts/json to dataframe

```py
bleh = [{'a':1,'b':5},{'a':2,'b':98},{'a':3,'b':45}]
pd.DataFrame(list(bleh))
```


# Extraction from tdapi

```py
"{'candles' : [{'id':0,'open':2,..},{'id':1,'open':6,..}]}

tempdf = pd.read_json(filein)
newdf = tempdf["candles"]
newdf2 = pd.DataFrame(list(newdf))
return newdf2
```


# Merging list of dataframes

```py
aa = [df,df3,df2,df4,df5]
def recmerge(xx):
    if len(xx) == 2:
        return pd.merge(xx[0],xx[1],how='outer')
    else:
        IH = xx[1:]
        return pd.merge(xx[0],recmerge(IH),how='outer')
```