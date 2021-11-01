---
title: Why Hakyll
tags: musings
---

$$ f(Hello) = World  $$ 

I've found the best way to retain knowledge is by taking notes. Pen and paper is best but I always end up losing my notes so I turned to text documents.  

Well, whenever I'm not on my PC and need to reference my notes, I am at a loss.  
Next solution, Google docs. It's a huge RAM eater and no LaTeX.    
Next solution, Gitbooks. Actually a pretty good solution but it gets tedious having to make screencaps to insert diagrams.  
CMS solution with Gatsby? The hosting isn't really free and you'll end up with a dead page if you go AWOL.  
And now here we are after a deep dive into functional programming.  

The great part is that I can just type my PlantUML(+ GraphViz) and Railroad Syntax diagrams in my markdown files instead of the tedious process of going to draw.io, drawing the diagram then saving it as a img.

#### My Hakyll Setup

1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)


```plantuml
@startuml

Bob -> Alice: Hello
Alice -> Bob: World
@enduml
```

```plantuml
digraph G {
    Hello -> World
} 
```

```rroad
Diagram(
    "Hello",
    Optional("World","skip")
)
```