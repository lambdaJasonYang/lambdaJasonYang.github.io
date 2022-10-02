---
title: Monitor Phone Packets
tags: prog, sec
---

# Monitoring Phone packets

## On router

* ~/phonePCAP/
  * scrape
  * close
  * sendfile
  * capfile0


```{.bash filename=scrape}
#!/bin/sh
tcpdump -i igb1 host 192.168.1.101 -W 10 -C 1G -w /root/phonePCAP/capfile
```

```{.bash filename=scrape}
#!/bin/sh
tar cv ./capfile0 | nc 192.168.1.245 7676
```

```{.bash filename=close}
#!/bin/sh
pkill -f ./scrape
pkill -f tcpdump
```

```{.bash filename=crontab}
58      7       *       *       *       /root/phonePCAP/close
59      7       *       *       *       /root/phonePCAP/sendfile
0       8       *       *       *       /root/phonePCAP/scrape
```

## On Server

* ~/0PhonePackets/
  * ncServer
  * 2022-09-22-11./
    * capfile0
  * 2022-09-23-11./
    * capfile0

```{.bash filename=ncServer}
#!/bin/zsh
wae=$(date '+%Y-%m-%d-%H')
ncat -l -p 7676  | tar xv --transform "s,^,$wae,"
```

```bash
58 7 * * * timeout 120s /home/rhel/0PhonePackets/ncServer
```

# Manually

from server

```bash
timeout 20s ./ncServer
```

from router

```bash
./sendfile
```
