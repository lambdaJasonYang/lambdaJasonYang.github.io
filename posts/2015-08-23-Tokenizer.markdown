---
title: Tokenizer
tags: mathcs, logic
---


Tokenize a math expression  
eg. "-2 * 4" => ["-2", "*", "4"]

```python
isspace = lambda k : k == ' ' or k == '\t'

def is_number(k):
    try:
        float(k)
        return True
    except ValueError:
        return False

def token_single(x):
    if len(x) == 0:
        return []
    elif len(x) == 1:
        return [x]
    elif isspace(x[0]):
        return token_single(x[1:])
    elif x[0].isdigit() or x[0] == '-':
        if x[0].isdigit() and isspace(x[1]):
            return [x[0]] + token_multi(x[2:])
        else:
            t = token_multi(x[1:])
            if t == []:
                return [x[0]]
            else:
                if is_number(t[0]):
                    t[0] = x[0] + t[0]
                    return t
                else:
                    return [x[0]] + t
    else:
        if x[0] == '(':
            t = token_single(x[1:])
        else:
            t = token_multi(x[1:])
        return [x[0]] + t

def token_multi(x):
    if len(x) == 0:
        return []
    elif len(x) == 1:
        return [x]
    elif isspace(x[0]):
        return token_multi(x[1:])
    elif x[0].isdigit():
        if isspace(x[1]):
            return [x[0]] + token_multi(x[2:])
        else:
            t = token_multi(x[1:])
            if t == []:
                return [x[0]]
            else:
                if is_number(t[0]):
                    t[0] = x[0] + t[0]
                    return t
                else:
                    return [x[0]] + t
    else:
        if x[0] == '(':
            t = token_single(x[1:])
        else:
            t = token_multi(x[1:])
        return [x[0]] + t

token_multi('-2')
```
