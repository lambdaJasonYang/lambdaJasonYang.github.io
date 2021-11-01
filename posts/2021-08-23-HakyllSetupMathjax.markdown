---
title: Adding Mathjax to Hakyll in 2021
tags: tech, prog, HakyllSetupSeries
---
#### Hakyll Setup Series
1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)


### JS setup

Add mathjax js to \<head\> in /templates/default.html   

Insert line 4,5,6 to your footer 
```{.html .numberLines filename="templates/default.html"}
<body>
...
<footer>
    <script id="MathJax-script" async
        src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js">
    </script>
</footer>
</body>

```

### Add dependencies
Modify your *.cabal file

Add "pandoc, containers" under build-depends in myblog.cabal  

Insert line 7,8
```{.sh .numberLines filename="myblog.cabal"}
executable myblog
  ...
  ...
  build-depends:       
    base >= 4.7 && < 5,
    hakyll,
    pandoc,
    containers   
```

### Setup functions in site.hs
```{.haskell filename="site.hs"}
   
--Step 0: Add "import Text.Pandoc.Options" in site.hs
import Text.Pandoc.Options
--..
--Step 1: Get the mathjax Extensions that recognizes single $ in our pandocs
mathjaxExtensions :: Extensions
mathjaxExtensions = extensionsFromList 
                    [Ext_tex_math_dollars --  $...$ or $$...$$
                    ,Ext_tex_math_double_backslash --  \(...\) or \[...\]
                    ,Ext_latex_macros
                    ,Ext_inline_code_attributes 
                    ]
--Step 2: Setup ReaderOptions using the Extensions from Step 1
readMathjaxOptions :: ReaderOptions 
readMathjaxOptions = defaultHakyllReaderOptions
                {
                    readerExtensions = (readerExtensions defaultHakyllReaderOptions) <> mathjaxExtensions
                }
--Step 3: Setup WriterOptions
writeMathjaxOptions :: WriterOptions
writeMathjaxOptions = defaultHakyllWriterOptions 
                {
                    writerHTMLMathMethod = MathJax ""
                }
--Step 4: Build the compiler using the ReaderOption and Writer Option from Step 2, 3.
mathJaxAddedCompiler :: Compiler (Item String)
mathJaxAddedCompiler = pandocCompilerWith readMathjaxOptions writeMathjaxOptions
```
### Modifying compiler in site.hs
Replace the line `compile $ pandocCompiler`{.haskell} with   
`compiler $ mathJaxAddedCompiler`{.haskell}  
As shown in line 8
``` {.haskell filename="site.hs"}
...
main :: IO ()
main = do    
    hakyllWith config $ do
        ...
        match "posts/*" $ do
            route $ setExtension "html"
            compile $ mathJaxAddedCompiler
               ...

```

remember to call 
```bash
stack build
stack exec myblog rebuild
```
to rebuild your site.hs  
replace "myblog" with the name of your hakyll project

### Conclusion
After doing all this you should be able to simply write your latex using  
```{.md}
$$ x \in Set $$
```
 $$ x \in Set $$


#### Aside
You can just do the first part(add mathjax js to \<head\>) for a working mathjax BUT this would make it unportable to other editors.

```{.tex}
\\[ x \\in Set \\]
```