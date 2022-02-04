---
title: BootLoader
tags: prog, OS
toc: y
---

# Summary

## Simple

1. BIOS reads first 512 bytes  
2. Check for magic 2-byte at end  
3. Loads the 512 bytes

## Extra

* The 512 bytes is in the first boot sector of the bootable drive.
* The magic 2-bytes is `0x55 0xaa`

# Implement

Using nasm x86 asm

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

Boot into the bootsector we enter into "REAL MODE".  
"REAL MODE" is a 16 bit mode assisted by BIOS.  


1. Switch to teletype mode
by modifying a register then calling a BIOS interrupt

```asm
mov ah, 0x0e
mov al, 'A'
int 0x10
```

Printing 'A's.
'A' = 65 in dec = 0x41 in hex = 0b1000001 in binary(0b means binary)

```asm
mov ah, 0x0e
mov al, 'A'
int 0x10

mov ah, 0x0e
mov al, 65
int 0x10

mov ah, 0x0e
mov al, 0x41
int 0x10

mov ah, 0x0e
mov al, 0b1000001
int 0x10

```

# Disks

Memory is divided into 512 chunks.

Cylinder = [0..)
Head = [0..)
Sector = [1..)

The location of each 512 chunk of memory on a disk is determined by  
(Cylinder,Head,Sector)

The first chunk of memory = "Boot Sector" at (C=0,H=0,S=1)


### OS

We need to write a bootloader.  
To compiler