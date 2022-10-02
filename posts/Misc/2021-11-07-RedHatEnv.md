---
title: Setting up Dev Env in Red-hat linux
tags: prog
toc: y
---

1. RHEL makes 2 accounts, the root and your default user which i named rhel.

2. by default rhel has no sudo privilege so first login to root then give sudo to rhel.

3. nano /etc/sudoers
under `root     ALL=(ALL)       ALL`
add   `rhel     ALL=(ALL)       ALL`



# Theory and Network

* NetworkManager is the core hub
* nmcli is used to talk with NetworkManager
* Even if you connect ethernet physically, you still need to set-up the config connection to get internet


nmcli con add con-name example ifname eno1 type ethernet ip4 192.168.1.86/24 gw4 192.168.1.1 
nmcli con mod example ipv4.dns "8.8.8.8"
nmcli -p con show example
nmcli connection up example

nmcli connection delete example

# Updating

* RHEL requires we "register" our system with our account
* `subscription-manager register --username <username> --password <password> --auto-attach`
* go to `https://www.redhat.com/wapps/tnc` and click agree
* `subscription-manager attach --auto`
* Cant update with `dnf` due to subscription error caused by NTP
* go to "192.168.1.86:9090" >> Services >> NTP client/server chronyd.service , Enable

## package update

* note dnf is called "dandified yum" aka better yum
* instead of apt-get, use dnf
* Search for package using `dnf search bleh`, `dnf search --all bleh`
* `dnf list --installed`
* `dnf update`

# Mounting drives and transferring files

* `lsblk`  -which show drives and partitions  
* `sudo mount /dev/sdc2 /mnt`


# Setup

## Code-server

Remember to change the configs following https://userjy.github.io/posts/2021-11-10-CodeServer.html

## firewalld

* code-server wont work because of firewall
* `sudo firewall-cmd --set-default-zone trusted`



# bashrc zshrc

```bash
#lean prover
export PATH="$PATH:$HOME/.elan/bin"
export NEXT_TELEMETRY_DISABLED=1

source ~/.zprofile
alias kubectl="minikube kubectl --"
export SAM_CLI_TELEMETRY=0

```

```txt
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/kali/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/kali/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/kali/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/kali/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

export PATH="$PATH:/home/kali/.local/bin"
export PATH="$HOME/.elan/bin:$PATH"
```


# Install docker

```bash
sudo dnf config-manager --add-repo=https://download.docker.com/linux/centos/docker-ce.repo
sudo dnf remove runc
sudo dnf install docker-ce
sudo dnf install docker-compose-plugin
sudo systemctl enable docker
sudo groupadd docker
sudo usermod -aG docker $USER
sudo chmod 666 /var/run/docker.sock
```

# Wolfram

* follow the instructions
* also chmod -R 777 the wolfram engine

```bash
sudo dnf install alsa-lib
```

