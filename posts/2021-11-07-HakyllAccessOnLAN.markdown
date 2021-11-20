---
title: Access Hakyll website on LAN and iptables
tags: prog
---

**Hakyll Setup Series**  

1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)
6. [Table Of Content in Hakyll](2021-10-01-TableOfContent.html)
7. [Hakyll Access on LAN server](2021-11-07-HakyllAccessOnLAN.html)



1. Hakyll website listens on 127.0.0.1 port 8000  
2. Client in LAN must hit 192.168.1.245 port 8000 to see the website    
3. Hakyll 127.0.0.1 in 8000 cant be seen in LAN  

Hakyll must open 127.0.0.1 port 8000 for LAN   

```bash
sudo sysctl -w net.ipv4.conf.all.route_localnet=1
sudo iptables -t nat -I PREROUTING -p tcp -d 192.168.1.0/24 --dport 8000 -j DNAT --to-destination 127.0.0.1:8000
```



![](https://www.karlrupp.net/en/computer/computer/graphics/nat-chains.gif)


# iptables netstat diagnosis

## netstat

```bash
sudo netstat -lnp

#tcp        0      0 0.0.0.0:8787            0.0.0.0:*               LISTEN      734/rserver 
#tcp        0      0 127.0.0.1:8000          0.0.0.0:*               LISTEN      149480/myblog 
```

## iptables

```bash
sudo iptables -L
```

```bash
### list INPUT rules
sudo iptables -L INPUT --line-numbers

### delete rule 2
sudo iptables -D INPUT 2
```

# Occupied port scenario

Say you launch a server to listen on port 8000  
but ERROR: port 8000 being used  

first check what process is running on port 8000 
```bash
sudo netstat -lnp
```

next check what the parent process is
```bash
ps -aef --forest #outputs PID ParentPID tree
```

terminate the process or it's parent
```bash
kill -9 <PID>
```