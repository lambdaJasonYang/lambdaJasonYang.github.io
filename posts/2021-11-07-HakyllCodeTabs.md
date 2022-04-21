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
  * Codeblock < datagroup < Grouplabels < Allblocks
  * `AllBlocks`: All blocks of code regardless if they are tabbed or not
    * `grouplabels` is the set of all unique group names in the document
      * `datagroup` is the set of codeblocks associated with a SINGLE grouplabel
        * `codeblock` is one of the codeblock within a `datagroup`

```{.js group="script" glabel="new"}
const AllBlocksPre = document.querySelectorAll("[data-group]");
const AllBlocks = [...AllBlocksPre]; //gets all codeblocks w/ and w/o group label
const getUniqueSet = (TargetSet,dataAttr) => {
    //gets the set of attributes of an array of codeblocks aka TargetSet
    const temp = TargetSet.map((e) => (e.getAttribute(dataAttr))); 
    const temp2 = temp.filter((a)=>a); //remove nulls
    return [...new Set(temp2)];
} 
const datagroupSet = getUniqueSet(AllBlocks,"data-group") //remove nulls

const getCodeBlocks = (datagroup) => {
    //return list of glabels CodeBlocks associated to a single group 
    return AllBlocks.filter((dataglabelBlock)=>(dataglabelBlock.getAttribute("data-group") === datagroup));
}

const showBlocks = (dataglabeltxt,datagroupCodeBlocks) => {
    const selectedglabelGroup = datagroupCodeBlocks.filter((SingleBlock)=>(SingleBlock.getAttribute("data-glabel") === dataglabeltxt))
    const NONselectedglabelGroup = datagroupCodeBlocks.filter((SingleBlock)=>(SingleBlock.getAttribute("data-glabel") !== dataglabeltxt))
    selectedglabelGroup.map((SingleBlock) => (SingleBlock.style.display="block"));
    (NONselectedglabelGroup || []).map((SingleBlock) => (SingleBlock.style.display="none"));
}
const mkBtn = (dataglabeltxt,datagroupCodeBlocks,showfunc) => {
    const newbutton = document.createElement("button");
    newbutton.textContent = dataglabeltxt;
    newbutton.addEventListener('click', ()=>{
        showfunc(dataglabeltxt,datagroupCodeBlocks);
    });
    return newbutton;
}
const showAll = (datagroup) => {
    //make all codeblocks visible
datagroup.map((e)=>(e.style.display="block"));
}

const buildCodeTab = (datagroupCodeBlocks) => {
    const leader = datagroupCodeBlocks[0]; //get the leader codeblock of a group of codeblock
    const setglabelstxt = getUniqueSet(datagroupCodeBlocks,"data-glabel"); 
    setglabelstxt.map((singleglabeltxt)=>{
        const btn = mkBtn(singleglabeltxt,datagroupCodeBlocks,showBlocks)
        leader.insertAdjacentElement("beforebegin",btn);

    })
    
    //make showAll button START
    const btnShowAll = document.createElement("button");
    btnShowAll.textContent = "All";
    btnShowAll.addEventListener('click',(e)=>(showAll(datagroupCodeBlocks)));
    leader.insertAdjacentElement('beforebegin', btnShowAll);
    //make showAllbutton END
}

//below code is performing actual behavior, the above code are just functions
datagroupSet.map((datagroup) => {
    const groupOfCodeblocks = getCodeBlocks(datagroup);
    buildCodeTab(groupOfCodeblocks);
    const firsttab = groupOfCodeblocks[0]
    showBlocks(firsttab.getAttribute("data-glabel"),groupOfCodeblocks);

})
```

```{.js group="script" glabel="old"}
 //Structure:
// Codeblock < Group/Grouplabel/datagroup < grouplabels < allblocks

const AllBlocksPre = document.querySelectorAll("[data-group]");
const AllBlocks = [...AllBlocksPre]; //gets all codeblocks w/ and w/o group label

const rawgrouplabels = AllBlocks.map((e)=>(e.getAttribute("data-group"))); //group label w/ repeats
const grouplabelsPre = [...new Set(rawgrouplabels)]; //get set of unique group labels
const grouplabels = grouplabelsPre.filter((a)=>a); //remove nulls

const getCodeBlocks = (datagroup) => {
    //return list of codeblocks associated to a single group label
    return AllBlocks.filter((dataglabel)=>(dataglabel.getAttribute("data-group") === datagroup));
}



const showSingleCode = (dataglabel,datagroup) => { 
    //make single codeblock visible
    dataglabel.style.display="block";
    const nonChosen = datagroup.filter((e)=>(e!=dataglabel));
    nonChosen.map((e)=>(e.style.display="none"));
}

const makeBtn = (dataglabel,datagroup,showfunc) => {
    //make button for a single codeblock
    const newbutton = document.createElement("button");
    newbutton.textContent=dataglabel.getAttribute("data-glabel");
    newbutton.addEventListener('click', ()=>{
        showfunc(dataglabel,datagroup);
    });
    return newbutton;
}

const showAll = (datagroup) => {
    //make all codeblocks visible
datagroup.map((e)=>(e.style.display="block"));
}

const buildCodeTab = (datagroup) => {
    const leader = datagroup[0]; //get the leader codeblock of a group of codeblock

    const btngrp =datagroup.map((dataglabel)=>(makeBtn(dataglabel,datagroup,showSingleCode)));
    btngrp.map((dataglabel)=>{
        leader.insertAdjacentElement('beforebegin',dataglabel);
        });
    
    //make showAll button START
    const btnShowAll = document.createElement("button");
    btnShowAll.textContent = "All";
    btnShowAll.addEventListener('click',(e)=>(showAll(datagroup)));
    leader.insertAdjacentElement('beforebegin', btnShowAll);
    //make showAllbutton END
}

//below code is performing actual behavior, the above code are just functions
grouplabels.map((datagroup) => {
    const groupOfCodeblocks = getCodeBlocks(datagroup);
    buildCodeTab(groupOfCodeblocks);
    showSingleCode(groupOfCodeblocks[0],groupOfCodeblocks) //default select the first block
})
      
```

# Usage

* To create a tabbed codeblock add a `group` parameter and `glabel` parameter.
  * `group` parameter aggregates the codeblocks to be grouped
    * if you have multiple tabbed groups of codeblocks then the `group` parameter must be unique for each.
  * `glabel` is the text shown on the tab

## Codeblocks


```{.c group="bleh" glabel="C++"}
cout << "bleh"
```
```{.py group="bleh" glabel="py"}
print("bleh")
```
```{.bash group="bleh" glabel="bash"}
echo "bleh"
```


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

## Fenced divs

::: {.hobbies group="hi" glabel="do"}
Helol
:::

::: {.hobbies group="hi" glabel="do"}
Goodbye 
:::

::: {.hobbies group="hi" glabel="do"}
``` plantuml
@startuml
Carl->Dan : Hi
@enduml
```
:::

::: {.hobbies group="hi" glabel="do2"}
Begin section
:::

::: {.hobbies group="hi" glabel="do2"}
``` plantuml
@startuml
Alice->Bob : I am using hex
@enduml
```
:::

::: {.hobbies group="hi" glabel="do2"}
End section
:::

This also works with fenced divs

```txt
::: {.hobbies group="hi" glabel="do"}
hafe
:::

::: {.hobbies group="hi" glabel="do"}
Begin section
:::

::: {.hobbies group="hi" glabel="do"}
''' plantuml
@startuml
Carl->Dan : Hi
@enduml
'''
:::

::: {.hobbies group="hi" glabel="do2"}
End section
:::

::: {.hobbies group="hi" glabel="do2"}
''' plantuml
@startuml
Alice->Bob : I am using hex
@enduml
'''
:::

::: {.hobbies group="hi" glabel="do2"}
hafe
:::
```