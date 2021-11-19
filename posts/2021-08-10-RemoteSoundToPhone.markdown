---
title: Remote Desktop Audio to phone
tags: prog
---

1. check if pulseaudio is installed

2. run bash commands

```bash
pactl load-module module-null-sink sink_name=rtp
pacmd 'update-source-proplist rtp.monitor device.description="Monitor of RTP"'
pacmd 'update-sink-proplist rtp device.description="RTP"'
```

3. The below command may need to be ran every time you stream audio so best if you made a bash file for it.  
192.168.1.179 is the IP of my phone


```bash
pactl load-module module-rtp-send source=rtp.monitor destination_ip=192.168.1.179 port=1234
```

4. go to pavucontrol or Pulse Audio Control Panel, go to output Devices >> RTP and enable

5. On mobile go to vlc app  
```bash
rtp://@:1234
```