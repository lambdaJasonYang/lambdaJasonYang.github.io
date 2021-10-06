---
title: HyperV on a home server
tags: tech, homeserver
---

### On Client Windows PC

`Get-NetConnectionProfile`{.bash}  
For me it returns "Wi-Fi" in the name field, use this in the next command.

`Set-NetConnectionProfile -InterfaceAlias Wi-Fi -NetworkCategory Private`{.bash}
`winrm quickconfig`{.bash}

`Set-Item WSMan:\localhost\Client\TrustedHosts -Value "192.168.1.66"`{.bash}
`Enable-WSManCredSSP -Role client -DelegateComputer "192.168.1.66"`{.bash}


Go to group policy
Computer Configuration > Administrative Templates > System > Credential Delegation  
Enable "Allow delegating fresh credentials", Press "Show..." next to "Add servers to the list:  
Add value "wsman/*"

Enable "Allow delegating fresh credentials with NTLM-only server authentication", Press "Show..." next to "Add servers to the list:  
Add value "wsman/*"  

Click on hyper-v manager "Connect to Server..."  
DESKTOP-ABCD2E\Administrator  
Your password to login to Administrator   

"DESKTOP-ABCD2E" will be replaced by the name of your PC.

## Bad Server Wifi startup fix
I noticed that my server never startups with the internet on and I have to reset the network adapter everytime which means physically being at the server PC and running commands.  

Here is the command to rest the Network adapter on Powershell.  
`Restart-NetAdapter (Get-NetAdapter | select -expand Name)`{.bash}  
 
##### Automate the wifi restart fix
To call the command on startup, we create a config file in powershell  

`New-Item -Path . -Name "Config.ps1" -ItemType "file" -Value "Restart-NetAdapter (Get-NetAdapter | select -expand Name)"`{.bash}  

Credit to stackoverflow "vkrams", a startup script that sets any task to run at startup.

```bash
$TaskAction1 = New-ScheduledTaskAction -Execute "PowerShell.exe" -Argument "-ExecutionPolicy Bypass -File Config.ps1"
$TaskTrigger = New-ScheduledTaskTrigger -AtStartup
$TaskPrincipal = New-ScheduledTaskPrincipal -UserID "NT AUTHORITY\SYSTEM" -LogonType ServiceAccount -RunLevel Highest
Register-ScheduledTask -Action $TaskAction1 -Trigger $TaskTrigger -Principal $TaskPrincipal -TaskName "Config" -Description "Config Script"
```

##### Irrelevant tidbits

The wget of linux in powershell is:
```bash
(New-Object System.Net.WebClient).DownloadFile("https://github.com/macchrome/winchrome/releases/download/v92.0.4515.107-r885287-Win64/92.0.4515.107_ungoogled_mini_installer.exe", "ChromeSetup.exe")
```
We can download a fully functioning Chrome on a barebones Hyper-V core server.
