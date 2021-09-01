---
title: Dependency Injection, Code Reflection
tags: prog
---
Many modern examples of dependency injection like ASP.net Core are actually more sophisticated than simple DI.
ASP.NET Core uses reflection.  
This allows us to simply add an interface and the DI system will automatically initialize a class to fill that interface.  

What is code reflection? It's a form of meta-programming, where your code understands it's own code.   
An example is python `eval("print('hello')")`{.python} which can evaluate strings as code.  
ASP.NET Core uses reflection which is the only way it could swap a concrete class with an interface without the programmer's "consent".

