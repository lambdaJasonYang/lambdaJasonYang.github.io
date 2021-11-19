---
title: Integrate PlantUML diagrams into Hakyll (Old)
tags: tech, prog, HakyllSetupSeries
---
** Go here [Integrate PlantUML diagrams into Hakyll (Updated)](2021-08-24-HakyllPlantUML2.html) that uses Hakyll libraries rather than my reinvent-the-wheel-bad implementations**

### Plant UML hex image link

Plant UML allows us to draw UML diagram using simple code.

Our goal is to allow us to write code in our pandocs markdown files in hakyll that will automatically generate the UML.  
PlantUML has a server that can convert links to image urls. 

``` bash
http://www.plantuml.com/plantuml/svg/~h407374617274756d6c0a416c6963652d3e426f62203a204920616d207573696e67206865780a40656e64756d6c
```


1. Converts code into ASCII decimal
2. Converts ASCII decimal into hex
3. Hex is part of URL of planttext which will generate our image 


``` haskell
strToASCII :: [Char] -> [Int]
strToASCII xs = fmap ord xs

asciiToHex :: [Int] -> [String]
asciiToHex xs = fmap (\x -> showHex x "") xs

plantUMLhex :: [Char] -> String 
plantUMLhex xs = (concat.  asciiToHex  . strToASCII) xs

-- replaceLF replaces markdown doublespace newlines hex with plantUML compatible newline hex
replaceLF :: T.Text -> T.Text 
replaceLF xs =  (T.replace "20200" "0a") xs

hexCode :: T.Text -> T.Text 
hexCode y = (replaceLF (T.pack ( plantUMLhex (T.unpack y))))

planthtml :: T.Text -> T.Text 
planthtml y = T.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (T.unpack $ hexCode y) <>"'></figure>") 
```
Above is our helper functions that will be used to generate the hex of our code.

example:  
INPUT Markdown codeblock content :  `@startuml`{.bash}  

1. `strToASCII`{.haskell} - Convert `@startuml`{.bash} to \[64,115,116,97,114,116,117,109,108\]
2. `asciiToHex`{.haskell} - Convert ASCII decimal encoding with \[40,73,74,61,72,74,75,6D,6C\]
3. `concat`{.haskell} - \[40,73,74,61,72,74,75,6D,6C\] to  "407374617274756D6C"
4. `replaceLF`{.haskell} - replace "20200" substrings with "0a", in this case there are none.
5. intermediate hex result: "407374617274756D6C"
6. `planthtml`{.haskell} - Use hex result to create a 'http://www.plantuml.com...' img src DOM string.

Notice in the plantuml image link above http://www.plantuml.com/plantuml/svg/~h407374617274756d6c...    
the string after "~h" begins with "407374617274756D6C" which is our result.


replaceLF is neccessary because the LineFeed of our Pandocs doesn't match PlantUML server's LineFeed. Therefore we design a function that replaces our LineFeed hex with a suitable hex.
This means in your markdown file you must add double space at the end of a line for a new line.
NOTE: Updated post fixes this problem [Integrate PlantUML diagrams into Hakyll (Updated)](2021-08-24-HakyllPlantUML2.html)

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
text  
```

Add to your site.hs

``` haskell
import           Text.Pandoc.Definition  
import           Text.Pandoc.Walk
import           Data.Text  

import Numeric (showHex)
import Data.Char (ord)
```

``` haskell
strToASCII :: [Char] -> [Int]
strToASCII xs = fmap ord xs

asciiToHex :: [Int] -> [String]
asciiToHex xs = fmap (\x -> showHex x "") xs

plantUMLhex :: [Char] -> String 
plantUMLhex xs = (concat.  asciiToHex . strToASCII) xs

-- replaceLF replaces markdown doublespace newlines hex with plantUML compatible newline hex
replaceLF :: T.Text -> T.Text 
replaceLF xs =  (T.replace "20200" "0a") xs

hexCode :: T.Text -> T.Text 
hexCode y = (replaceLF (T.pack ( plantUMLhex (T.unpack y))))

planthtml :: T.Text -> T.Text 
planthtml y = T.pack ("<figure><img src='http://www.plantuml.com/plantuml/svg/~h" <> (T.unpack $ hexCode y) <>"'></figure>") 

--Pandoc filtering, 
addToCodeBlock :: Pandoc -> Pandoc 
addToCodeBlock  = walk ftranslate 
  where ftranslate :: Block -> Block
        ftranslate (CodeBlock ("",["plantuml"],[]) txt ) = RawBlock (Format "html") (planthtml txt)
        ftranslate x = x 

```

* If you've followed my mathjax hakyll tutorial, simply add the code below:

```haskell
mathJaxAddedCompiler :: Compiler (Item String)
mathJaxAddedCompiler = pandocCompilerWithTransform readMathjaxOptions writeMathjaxOptions addToCodeBlock
```

* If you didn't follow my mathjax hakyll tutorial, then add the code below:
```haskell
simpleCompiler :: Compiler (Item String)
simpleCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addToCodeBlock
```