---
title: Linux and Networking
tags: tech, DevOps
---

```C
struct addrinfo {
        int              ai_flags;     // AI_PASSIVE, AI_CANONNAME, etc.
        int              ai_family;    // AF_INET, AF_INET6, AF_UNSPEC
        int              ai_socktype;  // SOCK_STREAM, SOCK_DGRAM
        int              ai_protocol;  // use 0 for "any"
        size_t           ai_addrlen;   // size of ai_addr in bytes
        struct sockaddr *ai_addr;      // struct sockaddr_in or _in6  <--- MOST INTERESTING
        char            *ai_canonname; // full canonical hostname
    
        struct addrinfo *ai_next;      // linked list, next node
    };
```

```C
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>

int main(int argc, char *argv[])
{
    struct addrinfo hints;
    struct addrinfo *servOutput; 
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;  
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    printf("hi\n");
    printf("%d\n",AF_UNSPEC,SOCK_STREAM);
    int status;

    status = getaddrinfo("www.example.com",NULL,&hints,&servOutput);
    printf("%d\n",status);
    printf("%d",servOutput-> ai_family);
    printf("%d",(*servOutput).ai_family);
    struct sockaddr_in* ipv4;
    //we typeforce since ai_addr :: sockaddir
    ipv4 = (struct sockaddr_in*) (*servOutput).ai_addr;
    struct in_addr temp = (*ipv4).sin_addr;
    struct in_addr* ptemp = &((*ipv4).sin_addr);
    //ip32bit = temp.s_addr;

    char ipstr[INET6_ADDRSTRLEN];
    printf("%d",temp.s_addr);
    inet_ntop((*servOutput).ai_family,&temp,ipstr, sizeof ipstr);
    printf(" %s",ipstr);

}
```