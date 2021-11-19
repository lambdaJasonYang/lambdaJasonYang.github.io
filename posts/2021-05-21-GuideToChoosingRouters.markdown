---
title: Guide to Choosing Routers
tags: tech, hardware
---
Guide for newcomers looking to become prosumers of routers.


What you want out of the router are :  
* Latency - higher mbps(megabytes per second) the better.
* Relability - no random disconnections
* Range - no dead zones

### Latency
Each section will have terms to look for which are keywords you will find on google or amazon searches.

#### 802.11 a/b/g/n Only routers 
* outdated.
* a/b/g/n only routers = 2.4Ghz and too slow
* Terms to look for: 802.11 abg, 802.11 abgn, 2.4 Ghz router

300mbps from an ISP will translate to around 80mbps to wifi devices in the same room and 40mbps on another floor.
They are typically only sold at second hand markets like ebay and are really cheap.
Best used as Dumb access points for IOT devices like cameras.

#### 802.11 ac Wave 1
* 802.11ac routers = 5Ghz channel + 2.4Ghz channel
* include the a/b/g/n bands 
* Terms to look for : 802.11 ac Wave 1, ac750
* outdated

#### 802.11 ac Wave 2 
* typically at the best price point and performance for 2021.
* Wave 2 = MU-MIMO 
* MU-MIMO means the router has better performance when multiple devices are connected to it.
  * MU-MIMO has it's own version of number of bandwidth streams that look like 2x2x2,3x3x3 or 4x4x4
* Terms to look for : MU-MIMO , 2x2x2,3x3x3,4x4x4,ac1200,ac1750 ac 1900, ac2100
* Most popular in households as of 2020

300 mbps from an ISP will translate to around 250 mbps to wifi devices in the same room to 100mbps on another floor.

#### 802.11 ax 
* It's being rolled out to the market right now.
* Terms to look for : 802.11 ax, AX, Wifi 6, ax3000

You may not need this if your device doesn't have a wifi 6 network card such as older labtops or PCs.  
However typically they have a better network chipset and thus better performance all-around.

#### Future: Wifi 6E

### Reliability
Your router should never be dropping wifi. 
Ideally big brands should test their products so this doesn't happen but they don't always do so well.
Netgear RAX200 is an example of a budget Wifi 6 router that disconnects at random.
Routers designed for foreign markets like Huawei or Xiaomi may not work as well here resulting in disconnections at random.
A possible solution is to flash the software with OpenWRT if a firmware exists or do some online googlefu with terms like "Netgear RAX200 disconnect".

### Range 
Look at antennas and power.

### Mesh Routers

A modern powerful dedicated 5 Ghz router in the $100 range is better than mesh routers. 

### Other links 

This website is helpful in comparing routers  
https://www.smallnetbuilder.com/

### My Setup
I have 
 * a PFsense that routes traffic 
 * a dedicated 5 Ghz TP-link router as a dumb AP for main internet usage
 * two OpenWrt 2.4 Ghz router meshed dumb AP for IOT devices

### On Second hand market Enterprise Access points
Routers like Ruckus, Aruba, Cisco, Aerohive
Like most enterprise products, much of the cost comes in factored into their service contract which you are not getting.
Even paying $200 would only net you a used Access point that's outdated with much slower performance compared to buying a new $200 Wifi 6 router.  
Believe me, I did it, specifically with a Ruckus R700. 
The performance and range did not even come close with a modern tp link.
