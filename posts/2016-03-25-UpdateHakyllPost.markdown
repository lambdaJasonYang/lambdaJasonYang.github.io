---
title: Updating Hakyll posts
tags: tech, HakyllSetupSeries
---

### How does Hakyll site.hs work?  
``` haskell
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
```

3 main functions: match route compile 

`match "images/*"`{.haskell}
images/dog.png will be matched

`route idRoute`{.haskell}   
route basically transforms the filename   
In this case idRoute is the identity function meaning no change to the name "dog.png"

`compile copyFileCompiler`{.haskell}
we copy "/images/dog.png" to the same path meaning no change.

### Updating hakyll posts

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

### Updating Hakyll site.hs

if you messed around in site.hs , you need to do
``` haskell
stack build
stack exec myblog rebuild
```

### Pandoc formatting

Example codeblock uses 3 backquotes then name of language  
we can also do {.haskell .numberLines} to have a numbered codeblock
``` {.haskell .numberLines}
--```haskell
--  fac n = foldr (*) 1 [1..n]
--```

--``` {.haskell .numberLines}
--fac n = foldr (*) 1 [1..n]
--```

```

``` haskell
fac n = foldr (*) 1 [1..n]
```


Inline code `print(stuff)`{.python} uses   
``` python
`print(stuff)`{.python}
```

   
  
For Latex 
```{.ruby .numberLines}
$$ bleh \in Set  $$
```
$$ bleh \in Set $$