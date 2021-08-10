---
title: 2021 Archer C7v2 Turning on Mesh
tags: tech, openwrt, homeserver
---


opkg remove ath10k-firmware-qca988x-ct kmod-ath10k-ct
opkg install ath10k-firmware-qca988x kmod-ath10k


Unknown package 'ath10k-firmware-qca988x'.
Unknown package 'kmod-ath10k'.
Collected errors:
 * opkg_install_cmd: Cannot install package ath10k-firmware-qca988x.
 * opkg_install_cmd: Cannot install package kmod-ath10k.


root@OpenWrt:~# opkg update
Downloading https://downloads.openwrt.org/releases/21.02.0-rc4/targets/ath79/gen        eric/packages/Packages.gz
Failed to send request: Operation not permitted
*** Failed to download the package list from https://downloads.openwrt.org/relea        ses/21.02.0-rc4/targets/ath79/generic/packages/Packages.gz


Go to your settings Interfaces > LAN >> Advanced Settings tab, 
Use Custom DNS servers: 8.8.8.8

then do opkg update and it should work
