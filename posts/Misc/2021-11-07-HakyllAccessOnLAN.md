---
title: Access Hakyll website on LAN and iptables
tags: prog
toc: y
---

**Hakyll Setup Series**  

1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)
6. [Table Of Content in Hakyll](2021-10-01-TableOfContent.html)
7. [Hakyll Access on LAN server](2021-11-07-HakyllAccessOnLAN.html)


# Scenario

1. Hakyll server: `listening on 127.0.0.1 port 8000...`{.bash}  
2. Client in LAN: must hit 192.168.1.245 port 8000 to see the website  
Client goes to http://192.168.1.245:8000 but sees ERROR    
3. Hakyll server: 127.0.0.1 in port 8000 cant be seen in LAN  

**GOAL** Hakyll server: must open 127.0.0.1 port 8000 for LAN   

# Solution

Solution 1:

```bash
stack exec -- site watch --host "0.0.0.0"
```

Solution 2:

```bash
sudo sysctl -w net.ipv4.conf.all.route_localnet=1
sudo iptables -t nat -I PREROUTING -p tcp -d 192.168.1.0/24 --dport 8000 -j DNAT --to-destination 127.0.0.1:8000
```

```bash
#list nat table rules
sudo iptables -t nat -L --line-numbers
```

```bash
#get nat table, select PREROUTING chain, delete number 2 
sudo iptables -t nat -D PREROUTING 2
```

* `-j DNAT` means rewriting Destination Network Address Translation aka IP rewriting.
* if any site is trying to connect to "192.168.1.245:8000"(`-p tcp -d 192.168.1.0/24 --dport 8000`),  
it will translate that address to "127.0.0.1:8000"(`-j DNAT --to-destination 127.0.0.1:8000`)  


# Theory

![](https://www.karlrupp.net/en/computer/computer/graphics/nat-chains.gif)

* `iptables -t filter ..` is default table w/ INPUT, FORWARD, OUTPUT    
  *  FORWARD chain is typically useless, it's when packets isnt directed towards you but passes through you like most router packets.
* `iptables -t nat ..` w/ PREROUTING, OUTPUT, POSTROUTING
  

# iptables netstat diagnosis



```bash
 sudo iptables --append INPUT --source 1.2.3.4 --jump DROP

sudo iptables --list
Chain INPUT (policy ACCEPT)
target     prot opt source               destination
DROP       all  --  1.2.3.4              anywhere
 ```
 
 * says append a rule to the INPUT chain and if a packet matches source 1.2.3.4 then DROP it
   * INPUT chain means all packets coming to the server is affected by this rule

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

## Rate limiting

* Rate limiting is not possible with vanilla iptables because it ratelimiting requires stateful connections
  * A hashmap maps "ConnectionSrc-ConnectionDst" to {"NEW","ESTABLISHED",...}

![](https://docs.google.com/drawings/d/e/2PACX-1vTu9nsWsRUn_a9IBpaKgBjGNGR14-AknVFs70pix7PFL2eQcqY1BagQztrH_duZDUleZ97Iq5vBnnPc/pub?w=1238&h=676)

* Using Connection state to rate limit

![](https://docs.google.com/drawings/d/e/2PACX-1vQgl3VdRoyti8ryMF-_NwUSgiDpdF88qGAKm3XTfAOLdV1_SOqeoJllLFtsLXSp5is9lxNHH7rlxd9H/pub?w=1297&h=554)


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