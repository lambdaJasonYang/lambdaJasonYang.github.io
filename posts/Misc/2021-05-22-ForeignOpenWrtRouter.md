---
title: 2021 Installing Openwrt
tags: tech, openwrt, homeserver
---

I will be installing Openwrt on black cylindrical Xiaomi mi router ac2100.

As of 2021, I've found the openwrt to be incorrect or outdated.
This guide will tell you how to flash your xiaomi mi router ac2100 to a snapshot build then upgrade to a stable build.   

Why not just flash directly to a stable build? Simply because I followed the Openwrt wiki then realized only later a stable newer build existed. yea... OpenWrt Wiki is not maintained well at all...


* For xiaomi mi ac2100 you must first downgrade the firmware.
  * If you have brand new mi router, you need to connect it to the internet and create a password and set up SSID.
    * Do not just connect to the default passwordless SSID that comes right out of the box because it will not work, I tried it.

Once you set up your ssid login to the router panel by going to `http://192.168.31.1`{.html}  

The Url should show  
`http://192.168.31.1/cgi-bin/luci/;stok=\<STOK\>/web/home#router`{.html}
 

For each of the below, replace \<STOK\> with your STOK
After each command you should see a page that display "Code: 0"

1) ``` html
http://192.168.31.1/cgi-bin/luci/;stok=\<STOK\>/api/misystem/set_config_iotdev?bssid=Xiaomi&user_id=longdike&ssid=-h%3Bnvram%20set%20ssh%5Fen%3D1%3B%20nvram%20commit%3B
```
2) ``` html
http://192.168.31.1/cgi-bin/luci/;stok=\<STOK\>/api/misystem/set_config_iotdev?bssid=Xiaomi&user_id=longdike&ssid=-h%3Bsed%20-i%20's/channel=.*/channel=%5C%22debug%5C%22/g'%20/etc/init.d/dropbear%3B
```
3) ``` html
http://192.168.31.1/cgi-bin/luci/;stok=<STOK>/api/misystem/set_config_iotdev?bssid=Xiaomi&user_id=longdike&ssid=-h%3B/etc/init.d/dropbear%20start%3B
```
4) ``` html
http://192.168.31.1/cgi-bin/luci/;stok=<STOK>/api/misystem/set_config_iotdev?bssid=Xiaomi&user_id=longdike&ssid=-h%3B%20echo%20-e%20'admin%5Cnadmin' %20%7C%20passwd%20root%3B
```

`ssh root@192.168.31.1`{.bash}

On windows, Download WinSCP
in WinSCP, connect to your router and select protocol "SCP"
Download snapshot files on your windows   
  
[kernel1.bin](http://downloads.openwrt.org/snapshots/targets/ramips/mt7621/openwrt-ramips-mt7621-xiaomi_mi-router-ac2100-squashfs-kernel1.bin)  
[rootfs0.bin](http://downloads.openwrt.org/snapshots/targets/ramips/mt7621/openwrt-ramips-mt7621-xiaomi_mi-router-ac2100-squashfs-rootfs0.bin)

On WinSCP, on the remote router folder, go up one level to the root folder then go to "/tmp/" folder 
Then drag the two files you downloaded onto your windows machine into that the remote "/tmp/" folder

Then on your ssh session

``` bash
mtd write /tmp/openwrt-ramips-mt7621-xiaomi_mi-router-ac2100-squashfs-kernel1.bin kernel1

mtd write /tmp/openwrt-ramips-mt7621-xiaomi_mi-router-ac2100-squashfs-rootfs0.bin rootfs0

nvram set uart_en=1
nvram set bootdelay=5
nvram set flag_try_sys1_failed=1
nvram commit
```

Then disconnect your router by unplugging the power.
You may notice this portion of the guide differs from the OpenWRT wiki. 

Well that is because when I followed the wiki, everytime I ran the commands in their order, the router just factory reset back to the original mi firmware.

### Turning on the Wifi

* connect your PC to your router by ethernet cable

```bash
ssh root@192.168.1.1
uci show
uci set wireless.radio1.disabled='0'
uci commit wireless
wifi up
```
WARNING: THE DEFAULT SSID is OpenWrt
So if you had an old OpenWrt router setup already, be aware.

### Setting up a Web GUI interface

* Connect your router to the internet by ethernet
* Connect your PC to the router by wifi

Take note that there is no Luci web GUI installed on this snapshot build so you have to configure the router using command line.

```bash
opkg update
opkg install luci
```


### Setting up as AP Mode
My real router is pfsense so I always use the "routers" that I buy as dumb APs.

* Interface > General Settings > Protocol  
  * Enter a different static address from your router

* Interface > DHCP Server > General Settings  
  * Enable "Ignore Interface"

* IPv6 Settings 
  * Disable RA-Services
  * Disable DHCPv6-Services

* System> Startup
  * disable firewall
  * disable dnsmasq
  * disable odhcpd

IMPORTANT: Wait a few seconds, then disconnect the ethernet on your PC and connect it to the main router so that your OpenWRT AP is connected to your router.
This has to be done within 90 seconds or the settings will rollback.

If you misconfigured something that results in locking yourself out of access to the router,
simply factory reset your router and it will reset to default OpenWrt settings.

Note the default settings doesn't have wifi turned on automatically so you have to access your router by ethernet to turn it on.


* The next step is to change back your LAN interface, 
  * Select gateway as 192.168.1.1 which is usually the IP of your main router
  * subnet mask is typically 255.255.255.0
  * Then go to advanced settings,  
  enter 8.8.8.8 or 1.1.1.1 as Custom DNS servers.(Google DNS and Cloudflare DNS respectively).   
    * If you don't enter the custom DNS servers then you can't install packages and `opkg update`{.bash} will bring up an error.

### Some issues with OpenWRT
Your wifi SSID may not appear while searching for networks if you use the incorrect channels and channel width.
You have to keep playing around with the configuration.
If your wifi disappears, then you have to play around with the channels and channel width and country origin settings on your wifi radio tab.
