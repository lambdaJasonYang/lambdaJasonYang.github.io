---
title: BootLoader
tags: tech, prog, asm, C
---


A bootloader is 512 bytes, 
```bash
sudo apt install nasm
sudo apt-get install qemu-system

nasm -f bin boot.bin -o boot.asm
qemu-system-x86_64 -nographic boot.bin 
```
\$ dollar sign means current address  
double dollar is section start  
(\$-\$\$) = (current addr - section start) = length of previous code  
db means define byte  
Zero out 512 bytes, then add the 2byte bootloader identitifer 0x55 0xaa

```asm
jmp $
times 510-($-$$) db 0
db 0x55, 0xaa
```
510 - length of previous code = allows us to fill the memory to 510 bytes 