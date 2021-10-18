---
title: Installing Generic Wifi dongle on Kali/Debian/Linux
tags: tech, openwrt, homeserver
---
```bash
lsusb
#>Bus 002 Device 003: ID 0bda:c811 Realtek Semiconductor Corp. 802.11ac NIC b
```

Search google for "0bda:c811"

You'll find that this generic driver requires Realtek RTL8811CU which can be found from this [git repo](https://github.com/morrownr/8821cu)



Before following the README.md on the github page first you must install "bc" or else you'll run into errors later on.

```bash
sudo apt update
sudo apt-get install bc -y
```

Now you can just follow the bash commands below or you can just follow the README.md on the github page.

```bash
sudo apt update
sudo apt install -y linux-headers-$(uname -r) build-essential dkms git libelf-dev
mkdir ~/src
cd ~/src
git clone https://github.com/morrownr/8821cu.git
sudo chmod -R 777 8821cu
cd ~/src/8821cu
sudo ./install-driver.sh
sudo modprobe 8821cu
sudo reboot
```




