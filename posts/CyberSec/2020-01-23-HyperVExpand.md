---
title: HyperV Expand win 10 VM virtual hard drive space
tags: tech
---

On Hyper V  
First turn off your VM  
On the upper right side panel labeled "Actions"  
 
1. Edit Disk... >> Next
2. Locate Disk: click Browse..., select the "Hard Disk Image File" type from "C:\\Users\\Public\\Documents\\Hyper-V\\Virtual Hard Disks\\YourVMdiskName"
3. Next
4. Choose Action: select Expand
5. Next
6. type in total new size in GB
7. Next >> Finish
 
8. Start your VM
9. Download AOMEI Partition Assistant Freeware Free version
10. Select the "Unallocated" Partition, right click it and select "Merge Partitions"
11. Check box check "Unallocated" and check "C:"
12. Click OK
13. Click Apply
 
14. "Restart Into Windows PE Mode"
15. Click OK
 
16. Click "Restart Now"
 