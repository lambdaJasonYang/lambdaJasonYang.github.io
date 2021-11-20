---
title: Code-Server
tags: prog
---


Config location: ~/.config/code-server/config.yaml

# unsecure HTTP setup

Warning!! unsecure HTTP disables web view  
extensions that show "previews" won't work like Lean 

```{.yaml filename="~/.config/code-server/config.yaml"}
bind-addr: 0.0.0.0:443
auth: password
password: root
cert: true
```

# HTTPS setup

Self signing CA

```bash
# Allows code-server to listen on port 443.
sudo setcap cap_net_bind_service=+ep /usr/lib/code-server/lib/node
```

```{.yaml filename="~/.config/code-server/config.yaml"}
bind-addr: 0.0.0.0:443
auth: password
password: root
cert: true
```


# Extensions

go to [open-vsx.org](https://open-vsx.org/)

```bash
wget https://open-vsx.org/api/something/vscode-bleh/1.2.0/file/something.vscode-bleh-1.2.0.vsix
code-server --install-extension something.vscode-bleh-1.2.0.vsix
```
