---
title: Table Of Content in Hakyll
tags: tech, prog, HakyllSetupSeries
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

```md
---
title: TableOfContent
tags: tech, prog, HakyllSetupSeries
toc: y
---

...content body...
```

Any value (in our case `y`{.md}) given to `toc`{.md} metafield activates the table of content. 

# Setup

```{.hs filename="site.hs"}
--Step 3: Setup WriterOptions
writeMathjaxOptions :: WriterOptions
--writeMathjaxOptions = defaultHakyllWriterOptions
writeMathjaxOptions = defaultHakyllWriterOptions 
                {          
                    writerSectionDivs = True
                    ,writerNumberSections  = True
                    ,writerColumns = 130
                    , writerTableOfContents = True
                   , writerTOCDepth        = 3
                   , writerHTMLMathMethod = MathJax ""
                }

---------------------------------------------------TOC
tocTemplate :: Text.Pandoc.Template Data.Text.Text
tocTemplate =
    either error id $ either (error . show) id $
    runPure $ runWithDefaultPartials $
    compileTemplate "" "<div id=\"TOC\">$toc$</div>\n$body$"

writeTOCMathjaxOptions :: WriterOptions
writeTOCMathjaxOptions = writeMathjaxOptions{
                   
                    writerTemplate        = Just tocTemplate
                
}

---------------------------------------------------END TOC

--Step 4: Build the compiler using the ReaderOption and Writer Option from Step 2, 3.
mathJaxAddedCompiler :: Compiler (Item String)
mathJaxAddedCompiler = pandocCompilerWithTransform readMathjaxOptions writeTOCMathjaxOptions addToCodeBlock
--Step 5: Replace the line "compile $ pandocCompiler" under "match "posts/*" $ do" with 
--"compiler $ mathJaxAddedCompiler"

--exclude Table of Content
mathJaxAddedCompilerExTOC :: Compiler (Item String)
mathJaxAddedCompilerExTOC = pandocCompilerWithTransform readMathjaxOptions writeMathjaxOptions addToCodeBlock

```


```{.hs filename="site.hs"}
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            --Check TOC metadata field filled START
            ident <- getUnderlying                                 
            toc   <- getMetadataField ident "toc"             
            let chosenCompiler = case toc of
                    Nothing -> mathJaxAddedCompilerExTOC
                    Just _ -> mathJaxAddedCompiler                        
            --Check TOC metadata field filled END 
            chosenCompiler
                >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
                >>= relativizeUrls
```