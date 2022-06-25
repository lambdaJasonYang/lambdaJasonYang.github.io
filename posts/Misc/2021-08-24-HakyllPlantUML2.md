---
title: Integrate PlantUML diagrams into Hakyll (Updated)
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



# Understand the Process

Goal is to 

1. write plantuml code in markdown
2. pandoc encode our code into some base64 format 
3. process the base64 encoding into a plantuml image HTML element
4. pandoc codeblock will filter codeblocks and translate them into HTML elements 

``` bash
http://www.plantuml.com/plantuml/svg/~h407374617274756d6c0a416c6963652d3e426f62203a204920616d207573696e67206865780a40656e64756d6c
```

1. Converts code into ASCII decimal
2. Converts ASCII decimal into hex
3. Hex is part of URL of planttext which will generate our image 

# Cabal and Imports

## Cabal file

```{.bash filename="myblog.cabal"}
pandoc,
pandoc-types,
text,
base16-bytestring,
bytestring, 
```

## Imports to site.hs

```{.haskell filename="site.hs"}
import qualified Data.ByteString.Char8 --for convert str to bytestr plantuml, requires cabal 'bytestring'
import Data.ByteString.Base16 (encode, decode) -- for encoding plantuml, requires cabal 'base16-bytestring' 
import Text.Pandoc.Walk --for post-processing pandoc, requires cabal 'pandoc-types'
```

# site.hs

## Encoding and process to img

```{.haskell filename="site.hs"}
mhexCode :: Data.Text.Text -> String
mhexCode y = tail $ init ( show ( Data.ByteString.Base16.encode $ Data.ByteString.Char8.pack $ Data.Text.unpack y ))

planthtml :: Data.Text.Text -> Data.Text.Text 
--planthtml y = T.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (T.unpack $ hexCode y) <>"'></figure>") 
planthtml y = Data.Text.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (mhexCode $ y) <>"'></figure>") 

```
Above is our helper functions that will be used to generate the hex of our code.

example:  
INPUT Markdown codeblock content :  `@startuml`{.bash}  

1. `Data.ByteString.Base16.encode`{.haskell} - Convert `@startuml`{.bash} to "407374617274756D6C"
2. intermediate hex result: "407374617274756D6C"
3. `planthtml`{.haskell} - Use hex result to create a 'http://www.plantuml.com...' img src DOM string.

Notice in the plantuml image link above http://www.plantuml.com/plantuml/svg/~h407374617274756d6c...    
the string after "~h" begins with "407374617274756D6C" which is our result.


## Pandoc Codeblock filtering + translation

Now we need to modify site.hs so that Hakyll will transform a PlantUML code block into a html img that links to the Planttext generated image.  
We can do this with Hakyll's Pandocs Filtering.    
  

```{.haskell filename="site.hs"}

--Pandoc filtering, 
addToCodeBlock :: Pandoc -> Pandoc 
addToCodeBlock  = walk ftranslate 
  where ftranslate :: Block -> Block
        ftranslate (CodeBlock ("",["plantuml"],[]) txt ) = RawBlock (Format "html") (planthtml txt)
        ftranslate x = x 

```
`ftranslate (CodeBlock ("",["plantuml"],[]) txt )`{.haskell} pattern matches CodeBlock objects in pandoc and finds "plantuml" annotations denoted by the Attr object `("",["plantuml"],[])`{.haskell}  
The Content of our CodeBlock is pattern matched as `txt`{.haskell}.   

After pattern matching it converts it into a raw html block, and applies our "Code to Img DOM" transformation function `planthtml`{.haskell} on the content.


INPUT Markdown codeblock : 
```bash
'''plantuml
@startuml  
Alice->Bob : I am using hex  
@enduml
'''
```  
Output DOM element :  

``` plantuml
@startuml
Alice->Bob : I am using hex
@enduml
```


# Compiler modification

* If you've followed my [mathjax hakyll tutorial](2021-08-23-HakyllSetupMathjax.html), simply add the code below:

```{.haskell filename="site.hs"}
mathJaxAddedCompiler :: Compiler (Item String)
mathJaxAddedCompiler = pandocCompilerWithTransform readMathjaxOptions writeMathjaxOptions addToCodeBlock
```

* If you didn't follow my mathjax hakyll tutorial, then add the code below:
```{.haskell filename="site.hs"}
simpleCompiler :: Compiler (Item String)
simpleCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addToCodeBlock
```