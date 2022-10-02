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


```{.bash filename="template/search.toml"}
[input]
url_prefix = "$root$/"
files = [
$for(posts)$
{path = "$path$", url = "$url$", title = "$title$"},
$endfor$
]
```

```{.bash filename="site.hs"}
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

for rhel:

```bash
#on RHEL
./stork-amazon-linux build --input "./docs/searchindex.toml" --output "./docs/storksearch.st"
```

for debian:

```bash
./stork-ubuntu-20-04 build --input "./docs/searchindex.toml" --output "./docs/storksearch.st"  
```


# git merge conflict

typically you get a git merge conflict if you update your search index and want to merge to remote github.   
You want to take the local new stork search so do this.   

Attempt a gitmerge with vscode, then you should see this with `git status`  

```.txt
On branch main
Your branch and 'origin/main' have diverged,
and have 1 and 1 different commits each, respectively.
  (use "git pull" to merge the remote branch into yours)

Unmerged paths:
  (use "git add <file>..." to mark resolution)
        both modified:   docs/storksearch.st
```

```bash
git checkout --ours /home/rhel/userjy.github.io/docs/storksearch.st
git add .
```

then vscode should allow you to sync