---
title: Updating Hakyll posts
tags: scripts
---

How does Hakyll site.hs work?
3 main functions: match, route, compile  
match "images/*"   
images/dog.png will be matched

route someRoute  
route basically transforms the filename  
2099-01-01-title.markdown will be converted to site/2099-01-01-title.html

compile someCompiler
Compile converts the content in the file like using the pandoc compiler then moves the folder into the output directory



```bash
chcp 65001
stack exec myblog clean
stack exec myblog build
```
view the site by going to your ~/myblog/docs/index.html

```bash
git add .
git commit -m "some message"
git push origin main:main
```

Hot reload
``` haskell
stack exec myblog watch
```

if you messed around in site.hs , you need to do
``` haskell
stack build
stack exec myblog rebuild
```


Example code

``` haskell
fac n = foldr (*) 1 [1..n]
```

```{.ruby .numberLines}
def greet; "Hello, world!"; end
```
   
  
For Latex 
```{.ruby .numberLines}
\\[ bleh \\in Set ...latex Code \\]
```
\\[ \\ln x = \\int_{-\\infty}^x \\frac 1 y \\, dy . \\]