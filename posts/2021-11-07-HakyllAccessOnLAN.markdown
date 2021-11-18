---
title: Access Hakyll website on LAN and iptables
tags: prog
---


Hakyll website listens on 127.0.0.1 port 8000  
Client in LAN must hit 192.168.1.245 port 8000 to see the website    
Hakyll 127.0.0.1 in 8000 cant be seen in LAN  
Hakyll must open 127.0.0.1 port 8000 for LAN   

```bash
sudo sysctl -w net.ipv4.conf.all.route_localnet=1
sudo iptables -t nat -I PREROUTING -p tcp -d 192.168.1.0/24 --dport 8000 -j DNAT --to-destination 127.0.0.1:8000
```


go to [open-vsx.org](https://open-vsx.org/)

```bash
wget https://open-vsx.org/api/redhat/vscode-yaml/1.2.0/file/redhat.vscode-yaml-1.2.0.vsix
code-server --install-extension redhat.vscode-yaml-1.2.0.vsix
```

![](https://www.karlrupp.net/en/computer/computer/graphics/nat-chains.gif)


#### netstat diagnose

```bash
netstat -lnp

#tcp        0      0 0.0.0.0:8787            0.0.0.0:*               LISTEN      734/rserver 
#tcp        0      0 127.0.0.1:8000          0.0.0.0:*               LISTEN      149480/myblog 
```

#### iptables

```bash
sudo iptables -L
```

```bash
### list INPUT rules
sudo iptables -L INPUT --line-numbers

### delete rule 2
sudo iptables -D INPUT 2
```

