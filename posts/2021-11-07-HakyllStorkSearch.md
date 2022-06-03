---
title: Hakyll stork search
tags: prog
toc: y
---

**Hakyll Setup Series**  

1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)
6. [Table Of Content in Hakyll](2021-10-01-TableOfContent.html)
7. [Hakyll Access on LAN server](2021-11-07-HakyllAccessOnLAN.html)


```{filename="template/search.toml"}
[input]
url_prefix = "$root$/"
files = [
$for(posts)$
{path = "$path$", url = "$url$", title = "$title$"},
$endfor$
]
```

```{filename="site.hs"}
    create ["searchindex.toml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .||. "posts/*/*.md" .||. "posts/*/*.markdown") 
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    constField "root" root                   <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/search.toml" indexCtx
```

```bash
#on RHEL
./stork-amazon-linux build --input "./docs/searchindex.toml" --output "./docs/storksearch.st"
```