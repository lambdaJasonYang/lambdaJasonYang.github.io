---
title: Convert Enterprise printer to a prosumer printer
tags: tech, hardware
---

#### background

My workplace was upgrading their equipment so they let me take home their kyocera printer (manufactured in 2016). It's sticker price is ~$10,000 and used ones around the webs were ~$3000. 
Well, I got it for free as long as I could handle moving a 200 lb printer to my home. 

#### Guide


CUPS server  
Set up  

Configuration  
Select AppSocket/HP Jet Direct    
Enter "socket://169.233.52.233"    
use ip of your printer   
The ip address can be found by going through your kyocera network panel.   
use the kyocera ppd 6002i file  

#### A Better way if you are using OpenWrt 
This option allows you to use kyocera interface.  

Requires: 2 OpenWrt routers   
Mesh the 2 OpenWrt routers  
There are cheap archer c7's floating around ebay for ~$20 that can mesh their 2.5Ghz bands.  

Simply find the ip of the printer then use that as the url.   
You can download a phone app to search IPs in your LAN network or just look at the printer network settings.  



#### Toner

Drill a hole, buy a rubber plug and use that hole to refill the toner.

#### Conclusion
A 60 ppm(pages per minute) MFP printer doesn't come cheap and I don't think I 'll need to replace this printer anytime in the near or far future. 