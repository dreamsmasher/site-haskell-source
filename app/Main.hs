--------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes, BlockArguments, OverloadedStrings #-}
module Main where

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Core.Configuration
import Text.Pandoc
import Text.Pandoc.Extensions
import Data.Text qualified as T
import Data.Functor ((<&>))
import Data.Char (isUpper, isLower)
import Data.Tuple.Curry
import Control.Monad
import Control.Applicative
import Control.Lens (view, set, over, (^.), (%~), (.~))
import Hakyll.Core.Logger qualified as L
import Text.Printf (printf)
import CSS
import Contexts
import Utils
import Sass
--------------------------------------------------------------------------------

main :: IO ()
main = do
  baseCtx <- buildBaseCtx
  let postsCtx' = postsCtx <> baseCtx
  hakyllWith config do
    match (fromList ["CNAME", "site.webmanifest"]) do
        route idRoute
        compile copyFileCompiler 

    match "images/**" do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" do
        route   idRoute
        compile compressCssCompiler

    match "css/*.scss" do
        route sassRoute
        compile 
            $ getResourceFilePath -- for better error messages
            >>= runSassFile
            >>= withItemBody (pure . compressCss) 

    match (fromGlob "siteroot/*.md") do
        route moveToRoot
        compile  
            $ initialTransforms
            >>= customPandoc
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls'

    match "posts/*" $ buildPost postsCtx'
    match "drafts/*.md" $ buildPost postsCtx'

    mapM_ (\t -> uncurryN mkListPage t postsCtx' [baseCtx])
        [ ("archive.html", "posts/*", "posts", "Archives", "templates/archive.html") 
        , ("drafts.html", "drafts/*", "posts", "Drafts", "templates/drafts.html")
        ]

    match "siteroot/index.html" do
        route moveToRoot
        compile do
            posts <- take maxIndexPagePosts <$> (recentFirst =<< loadAll "posts/*")
            let indexCtx = listField "posts" postsCtx' (pure posts) 
                           <> funcFields
                           <> boolField "index" (const True) -- hack so that we default to regular title
                           <> notPost 
                           <> baseCtx 

            getResourceBody 
                >>= applyAsTemplate indexCtx 
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls'


    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration -- removed custom deploy for CircleCI

writerConfig :: WriterOptions
writerConfig = def { writerExtensions = customExts , writerTableOfContents = True}

readerConfig :: ReaderOptions
readerConfig = def { readerExtensions = customExts, readerStripComments = True }

initialTransforms :: Compiler (Item String)
initialTransforms = getResourceBody >>= applyAsTemplate funcFields

customPandoc :: Item String -> Compiler (Item String)
customPandoc = renderPandocWithTransform readerConfig writerConfig walkPandocAST
    -- my contribution to Hakyll :DDDD

customExts :: Extensions -- pandoc options
customExts = pandocExtensions `mappend` extensionsFromList 
    [ Ext_native_divs
    , Ext_literate_haskell
    , Ext_emoji
    , Ext_inline_code_attributes
    , Ext_inline_notes
    , Ext_example_lists
    , Ext_tex_math_single_backslash 
    ]

-- for debugging
traceComp :: Show t => t -> Compiler t
traceComp x = unsafeCompiler (liftM2 (>>) print pure x)

buildPost :: Context String -> Rules ()
buildPost postsCtx' = do
    route $ setExtension "html"
    compile 
        $ getResourceBody 
        >>= customPandoc
        >>= loadAndApplyTemplate postTemplate postsCtx'
        >>= loadAndApplyTemplate defTemplate postsCtx'
        >>= relativizeUrls'