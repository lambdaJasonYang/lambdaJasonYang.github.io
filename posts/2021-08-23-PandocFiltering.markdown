---
title: Hakyll Pandoc filtering
tags: tech, prog,HakyllSetupSeries
---
**Hakyll Setup Series**  

1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-HakyllGitAction.html)
4. [Very Simple Hakyll Pandoc Filtering Example](2021-08-23-PandocFiltering.html)
5. [Add Railroad Syntax to Hakyll](2021-10-01-RailroadSyntax.html)
6. [Table Of Content in Hakyll](2021-10-01-TableOfContent.html)
7. [Hakyll Access on LAN server](2021-11-07-HakyllAccessOnLAN.html)




Pandoc filtering and translation - similar to a compiler  
It will translate certain markdown strings you type up into another form.

Here I will show the simplest example:

Append a text "EOF" string to all of our codeblocks automatically  

```{.haskell filename="site.hs"}
import           Text.Pandoc 
import           Text.Pandoc.Walk
import           Data.Text  
```

```{.haskell filename="site.hs"}
addToCodeBlock :: Pandoc -> Pandoc 
addToCodeBlock  = walk ftranslate 
  where ftranslate :: Block -> Block
        ftranslate (CodeBlock attr txt ) = CodeBlock attr (txt <> "EOF")
        ftranslate x = x 
		
simpleCompiler :: Compiler (Item String)
simpleCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addToCodeBlock
```

```{.hs .numberLines filename="site.hs"}
main :: IO ()
main = do
    hakyllWith config $ do
    ...
      match "posts/*" $ do
                route $ setExtension "html"
                compile $ simpleCompiler --ONLY CHANGE THIS
                    >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
                    ...

```

# Showcase

For example in my hakyll folder, I create a new file "2099-01-01-NewBlogPost.markdown"
and the contents are 
```markdown
---
title: Hello World
tags: tech
---

This is my blog post.
'''python
print("Hello World")

'''

```


The codeblock will show:  

```python
print("Hello World")
EOF
```


# Extra 

Text refers to Data.Text.Text

CodeBlock Attr Text
Attr = (Text, [Text], [(Text, Text)])  

`CodeBlock (Text, [Text], [(Text, Text)]) Text`{.hs}

We can convert CodeBlock to Dom Elements with RawBlock.

RawBlock Format Text

example: 
`RawBlock (Format "html") Text`{.hs}