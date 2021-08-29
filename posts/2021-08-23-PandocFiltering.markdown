---
title: Hakyll Pandoc filtering
tags: tech
---

Pandoc filtering means when I type up a blog post,  
I can control whether I want some text to be transformed in some manner after Hakyll builds it.

Here I will show the simplest example:

Lets append a text "EOF" to all of our codeblocks.

```haskell
import           Text.Pandoc.Definition  
import           Text.Pandoc.Walk
import           Data.Text  
```

```haskell  
addToCodeBlock :: Pandoc -> Pandoc 
addToCodeBlock  = walk ftranslate 
  where ftranslate :: Block -> Block
        ftranslate (CodeBlock attr txt ) = CodeBlock attr (txt <> "EOF")
        ftranslate x = x 
		
simpleCompiler :: Compiler (Item String)
simpleCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addToCodeBlock
```

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
note: the triple single quotes `'''`{.python} above are actually backticks but pandocs won't let me escape triple backticks.

The python codeblock will show:  

```python
print("Hello World")
EOF
```


### Aside 

CodeBlock Attr Text
CodeBlock takes a Attr type and Text type.   
type Attr = (Text, [Text], [(Text, Text)])  

Text type is a Data.Text and holds the content of the codeblock. To convert to string do "unpack Text"

We can also convert CodeBlock to Dom Elements with RawBlock.

RawBlock Format Text

example: 
RawBlock (Format "html") Text