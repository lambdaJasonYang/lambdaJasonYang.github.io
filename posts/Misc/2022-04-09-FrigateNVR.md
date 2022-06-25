---
title: Setup Frigate NVR
tags: prog
---



Goal:  

* create frigate config file `~/.config/frigate/frigate.yml`
  * configure where MQTT server is
  * configure RTSP of camera is
  * most configuration for frigate done here
* create frigate Video folder 
  * Where to store recording
* create a MQTT config file
* create a docker compose for both frigate and MQTT
  * cd to this folder to startup frigate

# Frigate Config file

```bash
cd ~
cd .config #if not exists, mkdir .config
mkdir frigate
cd frigate
touch frigate.yml 
```

```{.yaml filename="~/.config/frigate/frigate.yml"}
mqtt:
  host: server.opnroot.com
  port: 1883
timestamp_style:
  position: "tl"
  format: "%m/%d/%Y %H:%M:%S"
  thickness: 3
ffmpeg:
  output_args:
    record: -f segment -segment_time 10 -segment_format mp4 -reset_timestamps 1 -strftime 1 -c:v copy -c:a aac
birdseye:
  # Optional: Enable birdseye view (default: shown below)
  enabled: False
cameras:
  camera_1: # <------ Name the camera
    ffmpeg:
      inputs:
        #- path: rtsp://admin:admin@192.168.1.132:554/11 # <----- Update for your camera
        - path: rtsp://admin:admin@192.168.1.132/11/h264major
          roles:
            - detect
            - rtmp
    rtmp:
      enabled: False # <-- RTMP should be disabled if your stream is not H264
    detect:
      width: 1920 # <---- update for your camera's resolution
      height: 1080 # <---- update for your camera's resolution
      fps: 8
    record: 
      enabled: True
    snapshots:
      enabled: True
    mqtt:
      timestamp: True
#-------------------------------------------------
  camera_2: # <------ Name the camera
    ffmpeg:
      inputs:
        - path: rtsp://admin:admin@192.168.1.126:554/live/ch0 # <----- Update for your camera
          roles:
            - detect
            - rtmp
    rtmp:
      enabled: False # <-- RTMP should be disabled if your stream is not H264
    detect:
      width: 1920 # <---- update for your camera's resolution
      height: 1080 # <---- update for your camera's resolution
      fps: 8
    record: 
      enabled: True
    snapshots:
      enabled: True
    mqtt:
      timestamp: True
#-------------------------------------------------
  camera_3: # <------ Name the camera
    ffmpeg:
      inputs:
        - path: rtsp://admin:admin@192.168.1.127:554/live/ch0 # <----- Update for your camera
          roles:
            - detect
            - rtmp
    rtmp:
      enabled: False # <-- RTMP should be disabled if your stream is not H264
    detect:
      width: 1920 # <---- update for your camera's resolution
      height: 1080 # <---- update for your camera's resolution
      fps: 8
    record: 
      enabled: True
    snapshots:
      enabled: True
    mqtt:
      timestamp: True
```

# Create frigate Video folder

```bash
cd ~
cd Videos
mkdir frigateMedia
```

# create a MQTT config file

```bash
cd ~
cd .config
mkdir mosquitto
cd mosquitto
mkdir config
touch mosquitto.conf
```

```{.yml filename=.config/mosquitto/config/mosquitto.conf}
persistence false
log_dest stdout
allow_anonymous true
connection_messages true
listener 1883 0.0.0.0
```

# docker-compose

```bash
cd ~
mkdir frigate
cd frigate
touch docker-compose.yml
```

```{.yml filename=~/frigate/docker-compose.yml}
version: "3.9"
services:
  MQTT:
    container_name: eclipse-mosquitto
    privileged: true
    image: eclipse-mosquitto
    volumes:
      - $HOME/.config/mosquitto/config/mosquitto.conf:/mosquitto/config/mosquitto.conf
    ports:
      - 1883:1883
      - 9001:9001
  frigate:
    container_name: frigate
    privileged: true # this may not be necessary for all setups
    image: blakeblackshear/frigate:stable-amd64
    shm_size: "64mb" # update for your cameras based on calculation above
    volumes:
      - /etc/localtime:/etc/localtime:ro
      - /home/kali/.config/frigate/config.yml:/config/config.yml:ro
      - /home/kali/Videos/frigateMedia:/media/frigate
      - type: tmpfs # Optional: 1GB of memory, reduces SSD/SD Card wear
        target: /tmp/cache
        tmpfs:
          size: 1000000000
    ports:
      - "5000:5000"
    depends_on:
      - "MQTT"
    environment:
      FRIGATE_RTSP_PASSWORD: "password"
```

```bash
docker-compose build 
docker-compose up
```

# Misc

```txt
ONVIF url
http://192.168.1.132:8080

1920x1080
rtsp://admin:admin@192.168.1.132:554/11

640x352
rtsp://admin:admin@192.168.1.132:554/12

http://192.168.1.127:8899

http://192.168.1.126:8899
rtsp://admin:admin@192.168.1.126:554/live/ch0
rtsp://admin:admin@192.168.1.126:554/live/ch1

http://192.168.1.127:8899
rtsp://admin:admin@192.168.1.127:554/live/ch0
rtsp://admin:admin@192.168.1.127:554/live/ch1
```