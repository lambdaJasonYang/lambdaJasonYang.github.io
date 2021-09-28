---
title: BootLoader
tags: tech, prog, asm, C
---

Using nasm x86 asm

The first thing PC does is look for the Boot Sector (first sector of a bootable drive).

A bootsector is 512 bytes, 
```bash
sudo apt install nasm
sudo apt-get install qemu-system

nasm -f bin boot.bin -o boot.asm
qemu-system-x86_64 -nographic boot.bin 
```
single dollar = current address  
double dollar = section start  
  
(\$-\$\$) = (current addr - section start) = length of previous code  
db := define byte  

Zero out 510 bytes, then add the 2-byte bootloader identifier 0x55 0xaa

```asm
jmp $
times 510-($-$$) db 0
db 0x55, 0xaa
```
510 - length of previous code = allows us to fill the memory to 510 bytes 

---

BIOS (vs UEFI)