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