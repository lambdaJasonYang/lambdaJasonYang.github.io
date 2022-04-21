---
title: Quick MQTT
tags: prog, QuickCode
---


create a `mosquitto.conf`

0.0.0.0 allows us to hit the MQTT inside docker from the LAN. Without it, no one can access it, not even the machine its running on since it has been containered.

```yaml
persistence false
log_dest stdout
allow_anonymous true
connection_messages true
listener 1883 0.0.0.0
```

```bash
docker run -it -p 1883:1883 -p 9001:9001 -v $HOME/mosquitto.conf:/mosquitto/config/mosquitto.conf eclipse-mosquitto
```

```bash
#consumer 
mosquitto_sub -h 127.0.0.1 -t topicX
```

```bash
#producer 
mosquitto_pub -h 127.0.0.1 -t topic -m "Hello
" -d
```