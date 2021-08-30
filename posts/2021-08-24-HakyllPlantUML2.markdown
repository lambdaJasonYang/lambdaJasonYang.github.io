---
title: Integrate PlantUML diagrams into Hakyll (Updated)
tags: tech, prog, HakyllSetupSeries
---
#### Hakyll Setup Series
1. [Setup Mathjax](2021-08-23-HakyllSetupMathjax.html)
2. [Setup PlantUML](2021-08-24-HakyllPlantUML2.html)
3. [Setup autobuild Hakyll site Git action CI](2021-06-28-Hakyll.html)

### Plant UML hex image link

Plant UML allows us to draw UML diagram using simple code.

Our goal is to allow us to write code in our pandocs markdown files in hakyll that will automatically generate the UML.  
How it works is PlantUML has a server that can convert links to image urls. 

``` bash
http://www.plantuml.com/plantuml/svg/~h407374617274756d6c0a416c6963652d3e426f62203a204920616d207573696e67206865780a40656e64756d6c
```


1. Converts code into ASCII decimal
2. Converts ASCII decimal into hex
3. Hex is part of URL of planttext which will generate our image 

```haskell
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16 (encode, decode)
import qualified Data.Text as T
```

``` haskell
mhexCode :: T.Text -> String
mhexCode y = tail $ init ( show ( encode $ C.pack $ T.unpack y ))

planthtml :: T.Text -> T.Text 
planthtml y = T.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (mhexCode $ y) <>"'></figure>") 

```
Above is our helper functions that will be used to generate the hex of our code.

example:  
INPUT Markdown codeblock content :  `@startuml`{.bash}  

1. `encode`{.haskell} - Convert `@startuml`{.bash} to "407374617274756D6C"
2. intermediate hex result: "407374617274756D6C"
3. `planthtml`{.haskell} - Use hex result to create a 'http://www.plantuml.com...' img src DOM string.

Notice in the plantuml image link above http://www.plantuml.com/plantuml/svg/~h407374617274756d6c...    
the string after "~h" begins with "407374617274756D6C" which is our result.


### Pandocs filtering

Now we need to modify site.hs so that Hakyll will transform a PlantUML code block into a html img that links to the Planttext generated image.  
We can do this with Hakyll's Pandocs Filtering.    
  

``` haskell

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

### Full Code

add under "build-depends" in your .cabal file
```bash
pandoc,
pandoc-types,
text,
base16-bytestring,
bytestring,  
```

Add to your site.hs

``` haskell
import           Text.Pandoc.Definition  
import           Text.Pandoc.Walk
import           Data.Text  

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16 (encode, decode)
import qualified Data.Text as T
```

``` haskell
mhexCode :: T.Text -> String
mhexCode y = tail $ init ( show ( encode $ C.pack $ T.unpack y ))

planthtml :: T.Text -> T.Text 
planthtml y = T.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (mhexCode $ y) <>"'></figure>") 

--Pandoc filtering, 
addToCodeBlock :: Pandoc -> Pandoc 
addToCodeBlock  = walk ftranslate 
  where ftranslate :: Block -> Block
        ftranslate (CodeBlock ("",["plantuml"],[]) txt ) = RawBlock (Format "html") (planthtml txt)
        ftranslate x = x 

```

* If you've followed my [mathjax hakyll tutorial](2021-08-23-HakyllSetupMathjax.html), simply add the code below:

```haskell
mathJaxAddedCompiler :: Compiler (Item String)
mathJaxAddedCompiler = pandocCompilerWithTransform readMathjaxOptions writeMathjaxOptions addToCodeBlock
```

* If you didn't follow my mathjax hakyll tutorial, then add the code below:
```haskell
simpleCompiler :: Compiler (Item String)
simpleCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addToCodeBlock
```