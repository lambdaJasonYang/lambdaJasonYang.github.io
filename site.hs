--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified GHC.IO.Encoding as E
import Text.Pandoc.Definition
    ( Block(RawBlock, CodeBlock), Format(Format), Pandoc )
import Text.Pandoc.Options
    ( extensionsFromList,
      Extension(Ext_inline_code_attributes, Ext_tex_math_dollars,
                Ext_tex_math_double_backslash, Ext_latex_macros),
      Extensions,
      HTMLMathMethod(MathJax),
      ReaderOptions(readerExtensions),
      WriterOptions(writerHTMLMathMethod) )

import Hakyll.Web.Tags (renderTagCloud)
import qualified Data.ByteString.Char8 as C
--import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode, decode)

import           Text.Pandoc.Walk
import qualified Data.Text as T
import GHC.IO.Handle.Types (BufferMode(BlockBuffering))

import Numeric (showHex)
import Data.Char (ord)


--------------
--------------
--------------------------------------------------------------------------------
--Setup Mathjax on Hakyll
--Step 0: Add "pandoc, containers" under build-depends in stack.yaml   
--Step 0: "import Text.Pandoc.Options" in site.hs
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
mathJaxAddedCompiler = pandocCompilerWithTransform readMathjaxOptions writeMathjaxOptions addToCodeBlock
--Step 5: Replace the line "compile $ pandocCompiler" under "match "posts/*" $ do" with 
--"compiler $ mathJaxAddedCompiler"
--------------------------------------------------------------------------------

--------------------------------------------- PLANT UML pandoc filter

strToASCII :: [Char] -> [Int]
--strToASCII xs = fmap ord ( filter (\x -> not $ isSpace x) xs )
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
        ftranslate x = x 

                

----------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith config $ do
        match "images/**" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match (fromList ["about.rst", "contact.markdown"]) $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
----------------------
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")
        tagsRules tags $ \tag tagpattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll tagpattern
                let ctx = constField "title" title
                        `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                        `mappend` defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls
----------------------
        match "posts/*" $ do
            route $ setExtension "html"
            compile $ mathJaxAddedCompiler
                >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
                >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                taglist <- renderTagCloud 90 130 tags
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        constField "taglist" taglist             `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateCompiler
--------Building sitemap
        create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])
                let pages = posts <> singlePages
                    sitemapCtx =
                        constField "root" root <> -- here
                        listField "pages" postCtx (return pages)
                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
--root is for sitemap
root :: String
root = "https://userjy.github.io"

postCtx :: Context String
postCtx =
    constField "root" root `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
