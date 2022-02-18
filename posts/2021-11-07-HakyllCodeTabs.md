---
title: Tabbed Code blocks in Hakyll
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

# Showcase

```{.c group="bleha" glabel="C++"}
cout << "bleh"
```
```{.py group="bleha" glabel="py"}
print("bleh")
```
```{.bash group="bleha" glabel="bash"}
echo "bleh"
```

# Instruction



Add the below javascript to your one of your templates like `default.html`

* Structure
  * Codeblock < subgrp < Grouplabels < Allblocks
  * `AllBlocks`: All blocks of code regardless if they are tabbed or not
    * `grouplabels` is the set of all unique group names in the document
      * `subgrp` is the set of codeblocks associated with a SINGLE grouplabel
        * `codeblock` is one of the codeblock within a `subgrp`

```js

const AllBlocksPre = document.getElementsByClassName("sourceCode");
const AllBlocks = [...AllBlocksPre]; //gets all codeblocks w/ and w/o group label

const rawgrouplabels = AllBlocks.map((e)=>(e.getAttribute("data-group"))); //group label w/ repeats
const grouplabelsPre = [...new Set(rawgrouplabels)]; //get set of unique group labels
const grouplabels = grouplabelsPre.filter((a)=>a); //remove nulls

const getCodeBlocks = (grouplabel) => {
    //return list of codeblocks associated to a single group label
    return AllBlocks.filter((codeblock)=>(codeblock.getAttribute("data-group") === grouplabel));
}

const showSingleCode = (codeblock,subgrp) => { 
    //make single codeblock visible
    codeblock.style.display="block";
    const nonChosen = subgrp.filter((e)=>(e!=codeblock));
    nonChosen.map((e)=>(e.style.display="none"));
}

const makeBtn = (codeblock,subgrp) => {
    //make button for a single codeblock
    const newbutton = document.createElement("button");
    newbutton.textContent=codeblock.getAttribute("data-glabel");
    newbutton.addEventListener('click', ()=>{
        showSingleCode(codeblock,subgrp);
    });
    return newbutton;
}

const showAll = (subgrp) => {
    //make all codeblocks visible
subgrp.map((e)=>(e.style.display="block"));
}

const buildCodeTab = (subgrp) => {
    const leader = subgrp[0]; //get the leader codeblock of a group of codeblock

    const btngrp =subgrp.map((e)=>(makeBtn(e,subgrp)));
    btngrp.map((e)=>{
        leader.insertAdjacentElement('beforebegin',e);
        });
    
    //make showAll button START
    const btnShowAll = document.createElement("button");
    btnShowAll.textContent = "All";
    btnShowAll.addEventListener('click',(e)=>(showAll(subgrp)));
    leader.insertAdjacentElement('beforebegin', btnShowAll);
    //make showAllbutton END
}

//below code is performing actual behavior, the above code are just functions
grouplabels.map((grouplabel) => {
    const groupOfCodeblocks = getCodeBlocks(grouplabel);
    buildCodeTab(groupOfCodeblocks);
    showSingleCode(groupOfCodeblocks[0],groupOfCodeblocks) //default select the first block
})
      
```

* To create a tabbed codeblock add a `group` parameter and `glabel` parameter.
  * `group` parameter aggregates the codeblocks to be grouped
    * if you have multiple tabbed groups of codeblocks then the `group` parameter must be unique for each.
  * `glabel` is the text shown on the tab

```md
<!-- fenced codeblocks should use triple backticks ``` 
instead of single quotes ''' which is used here for presentation purposes -->
'''{.c group="bleh" glabel="C++"}
cout << "bleh"
'''
'''{.py group="bleh" glabel="py"}
print("bleh")
'''
'''{.bash group="bleh" glabel="bash"}
echo "bleh"
'''
```

```{.c group="bleh" glabel="C++"}
cout << "bleh"
```
```{.py group="bleh" glabel="py"}
print("bleh")
```
```{.bash group="bleh" glabel="bash"}
echo "bleh"
```