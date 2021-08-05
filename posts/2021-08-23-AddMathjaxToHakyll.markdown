---
title: Adding Mathjax to Hakyll in 2021
tags: tech
---
### Add mathjax js to \<head\> in /templates/default.html   

Insert line 3,4,5
```{.html .numberLines}
<head>...

<script id="MathJax-script" async
    src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js">
</script>

...</head>

```

### Modify myblog.cabal
Note For you it is named _site.cabal or what you named your hakyll project.

Add "pandoc, containers" under build-depends in myblog.cabal  

Insert line 7,8
```{.sh .numberLines}
executable myblog
  ...
  ...
  build-depends:       
    base >= 4.7 && < 5,
    hakyll,
    pandoc,
    containers   
```

### Modify site.hs
```haskell
   
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
--Step 5: Replace the line "compile $ pandocCompiler" under "match "posts/*" $ do" with 
--"compiler $ mathJaxAddedCompiler"
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
You can just do the first part(add mathjax js to \<head\>) for a working mathjax BUT then you would have to escape characters frequently and this would make it unportable to other editors.

```{.tex}
\\[ x \\in Set \\]
```