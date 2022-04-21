---
title: Code-Server
tags: prog
---

Without HTTPS, SSL Cert, things like Coq, Lean, wont work. Extensions wont show details, etc. Codeserver 3.8.0 is the last version that works without SSL.


Config location: ~/.config/code-server/config.yaml

# HTTPS setup on Opnsense or Pfsense 

In the cert and cert-key fields, remember to use ABSOLUTE PATHS or systemctl wont work.

```bash
# Allows code-server to listen on port 443.
sudo setcap cap_net_bind_service=+ep /usr/lib/code-server/lib/node
```

```{.yaml filename="~/.config/code-server/config.yaml"}
bind-addr: 0.0.0.0:443
auth: password
password: root
cert: /home/kali/.local/share/code-server/VsCodeOpn.crt
cert-key: /home/kali/.local/share/code-server/VsCodeOpn.key
```

1. Next you have to generate your Certificate Root Authority. (This is what your browser downloads)

Trust >> Authorities >> Add

* Name: Web SSL Cert
* Method: Create an internal Certificate Authority
* Leave the Cipher and Hash default or else it wont work if key is too weak.
* Use valid city and state data

2. Next you have to generate your Server Certificate.  (This is what you put in your remote "~/.local/share/code-server/" directory)

Trust >> Certificates >> Add

* Name: VsCodeOpn
* Method: Create an internal Certificate
* Certificate Authority: Web SSL Cert
* Type: Server Certificate
* Alternative Names:
  * DNS: server.opnroot.com
  * IP: 192.168.1.245

Download the VsCodeOpn.crt and VsCodeOpn.key    
SSH into PC hosting your code-server, Move them to some directory like :  
~/.local/share/code-server/VsCodeOpn.crt  
~/.local/share/code-server/VsCodeOpn.key  

3. Go to Browser  

Trust >> Authorities

Download the Web-SSL-Cert.crt, which is the Root Cert Authority. Go to Chrome settings , Manage Certificates >> Authorities, and install the *.crt.  
Note we DO NOT need to install VsCodeOpn.crt on our browser.  





# Extensions

go to [open-vsx.org](https://open-vsx.org/)

```bash
wget https://open-vsx.org/api/something/vscode-bleh/1.2.0/file/something.vscode-bleh-1.2.0.vsix
code-server --install-extension something.vscode-bleh-1.2.0.vsix
```


# ssh

```bash
sudo apt-get install ssh
sudo nano /etc/ssh/sshd_config
#"PermitRootLogin prohibit-password" => "PermitRootLogin yes"
sudo systemctl enable ssh

```

# Lean and Multi-root workspaces

* To use multi-root vscode workspaces with lean, the *.workplace file must be placed inside a lean project folder. 


# Jupyter

```bash
env GO111MODULE=on go install github.com/gopherdata/gophernotes@latest
cd "$(go env GOPATH)"/src/github.com/gopherdata/gophernotes
env GO111MODULE=on go install
mkdir -p ~/.local/share/jupyter/kernels/gophernotes
cp kernel/* ~/.local/share/jupyter/kernels/gophernotes
cd ~/.local/share/jupyter/kernels/gophernotes
chmod +w ./kernel.json
sed "s|gophernotes|$(go env GOPATH)/bin/gophernotes|" < kernel.json.in > kernel.json
```