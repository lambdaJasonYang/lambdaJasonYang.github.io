---
title: Quick C++ Header and Doxygen
tags: prog, C, QuickCode, OS
---
 
```{.cpp filename=duck.h}
#ifndef DUCK_H
#define DUCK_H

class Iduck{
    public:
        virtual int a;
};

//!superduck
class duck: virtual public Iduck{
    //!massive duck
    public:
        int a;

};


class blueduck : public duck {
    public:
        int b;
};

#endif
```

# Doxygen

```bash
doxygen -g dconf #generate doxygen config
doxygen dconf #build doc HTML from doxygen config
```

Make sure to have graphviz's dot `sudo dnf install graphviz`

```{.txt filename=dconf}
CALL_GRAPH             = YES
UML_LOOK               = YES
HAVE_DOT               = YES
```
