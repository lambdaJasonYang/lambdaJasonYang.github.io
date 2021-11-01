---
title: Add Railroad Syntax to Hakyll
tags: tech, prog, HakyllSetupSeries
---
#### Hakyll Setup Series
1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)


### Setup

add the script to your templates/default.html

```{.html filename="templates\default.html"}
 <!-- RAILROAD START -------------------------------------- -->
        <script type="module">
            import rr,* as rrClass from "/lib/railroad/railroad.js";
            Object.assign(window,rr)
            window.rrOptions = rrClass.Options;
            document.addEventListener('DOMContentLoaded',()=>{ReplaceDivWithSvg()},false)
            const ReplaceDivWithSvg = () =>  {
                for (const railroadelem of document.getElementsByClassName("rroad") ){
                railroadelem.innerHTML = eval(railroadelem.innerText.trim()+".toString()")
                }
            }
        </script>
        

        <link rel="stylesheet" href="/lib/railroad/railroad-diagrams.css">
<!-- RAILROAD END ----------------------------------------- -->

```

add lines 8-10 to your site.hs

```{.hs .numberLines filename=site.hs}
--add lines 7-9 to your site.hs
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith config $ do
        ...

        match "lib/**" $ do
            route   idRoute
            compile copyFileCompiler
```

Create a lib directory and create a railroad directory within the newly created lib directory.

Create a file "railroad.js" inside you /lib/railroad directory and copy the contents from [this link](https://raw.githubusercontent.com/tabatkins/railroad-diagrams/gh-pages/railroad.js)

Create a file "railroad-diagrams.css" /lib/railroad directory and copy the contents from [this link](https://raw.githubusercontent.com/tabatkins/railroad-diagrams/gh-pages/railroad-diagrams.css)


**If you followed my [Setup PlantUML(Updated)](2021-08-24-HakyllPlantUML2.html)**

Line **3, 4, 20** is what we added to our previous plantuml-hakyll integration code.

```{.hs .numberLines}
------------------------------------------------RAILROAD START

railroad :: T.Text -> T.Text
railroad y = T.pack("<div class='rroad'>"<> T.unpack y <> "</div>")

------------------------------------------------RAILROAD END

mhexCode :: T.Text -> String
mhexCode y = tail $ init ( show ( encode $ C.pack $ T.unpack y ))

planthtml :: T.Text -> T.Text 
--planthtml y = T.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (T.unpack $ hexCode y) <>"'></figure>") 
planthtml y = T.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (mhexCode $ y) <>"'></figure>") 

--Pandoc filtering, 
addToCodeBlock :: Pandoc -> Pandoc 
addToCodeBlock  = walk ftranslate 
  where ftranslate :: Block -> Block
        ftranslate (CodeBlock ("",["plantuml"],[]) txt ) = RawBlock (Format "html") (planthtml txt)
        ftranslate (CodeBlock ("",["railroad"],[]) txt ) = RawBlock (Format "html") (railroad txt)
        ftranslate x = x 
```

Now it should work when you input codeblocks your pandoc markdown with "rroad" tag as shown below (use backticks instead of single quotes in your pandoc markdown).

```
'''rroad
Diagram("foo")
'''
```

```rroad
Diagram("foo")
```

---

**If you did NOT follow my [Setup PlantUML(Updated)](2021-08-24-HakyllPlantUML2.html)**

Add the codeblock below to your site.hs

```{.hs filename="site.hs"}
------------------------------------------------RAILROAD START

railroad :: T.Text -> T.Text
railroad y = T.pack("<div class='rroad'>"<> T.unpack y <> "</div>")

------------------------------------------------RAILROAD END

--Pandoc filtering, 
addToCodeBlock :: Pandoc -> Pandoc 
addToCodeBlock  = walk ftranslate 
        ftranslate (CodeBlock ("",["railroad"],[]) txt ) = RawBlock (Format "html") (railroad txt)
        ftranslate x = x 

--New document compiler
modAddedCompiler :: Compiler (Item String)
modAddedCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addToCodeBlock
```

In your site.hs find the `match "posts/*" $ do`{.hs} and ONLY change the compiler in line 8 to `modAddedCompiler`{.hs} 

```{.hs .numberLines filename="site.hs"}
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith config $ do
    ...
      match "posts/*" $ do
                route $ setExtension "html"
                compile $ modAddedCompiler --ONLY THIS
                    >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
                    ...

```

Now it should work when you input codeblocks your pandoc markdown with "rroad" tag as shown below (use backticks instead of single quotes in your pandoc markdown).

```
'''rroad
Diagram("foo")
'''
```

```rroad
Diagram("foo")
```