--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified GHC.IO.Encoding as E

import           Text.Pandoc.Options
import Hakyll.Web.Pandoc
import qualified  Text.Blaze.Html5                as H

--------------------------------------------------------------------------------
blogWriterOptions :: WriterOptions
blogWriterOptions = 
   defaultHakyllWriterOptions
      {
        writerHTMLMathMethod = MathJax ""
      , writerTableOfContents = True
      , writerNumberSections  = True
      , writerTOCDepth        = 2
      , writerTemplate        = 
         let
            toc = "$toc$" :: String
            body = "$body$" :: String
         in
            Just . renderHtml $ do
               H.div ! class_ "toc" $ do
                  toHtml toc
               toHtml body
      }
blogReaderOptions :: ReaderOptions
blogReaderOptions = 
   defaultHakyllReaderOptions
      {
         readerExtensions = 
            (readerExtensions defaultHakyllReaderOptions) <> extensionsFromList
               [ 
                 Ext_tex_math_single_backslash  -- TeX math btw (..) [..]
               , Ext_tex_math_double_backslash  -- TeX math btw \(..\) \[..\]
               , Ext_tex_math_dollars           -- TeX math between $..$ or $$..$$
               , Ext_latex_macros               -- Parse LaTeX macro definitions (for math only)
               , Ext_inline_code_attributes     -- Ext_inline_code_attributes

               ]
      }

blogCompiler :: Compiler (Item String)
blogCompiler = do
   ident <- getUnderlying
   toc   <- getMetadataField ident "withtoc"
   pandocCompilerWith blogReaderOptions (maybe defaultOptions blogOptions toc)
   where
      defaultOptions = defaultHakyllWriterOptions
      blogOptions = const blogWriterOptions

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith config $ do
        match "images/*" $ do
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

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
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


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

