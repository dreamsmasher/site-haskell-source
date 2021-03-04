--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Core.Configuration
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Pandoc
import           Text.Pandoc.Extensions
import           Data.List
import           Control.Monad
import qualified Hakyll.Core.Logger as L
import           Control.Monad.IO.Class
import           CSS
import           Contexts
--------------------------------------------------------------------------------

main :: IO ()
main = do
  hakyllWith config $ do

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- create ["css/default.css"] $ do
    --     route idRoute
    --     compile $ do
    --         let css = bodyField (compressCss renderSiteCSS)
    --                 <> defaultContext
    --         makeItem ""
    --             >>= 
    -- match "css/default.css" $ do
    --     route idRoute
    --     compile $ do
    --         let css = bodyField (compressCss renderSiteCSS)
    --         getResourceBody
    --         >>= applyAsTemplate (bodyField $ compressCss renderSiteCSS) -- turn our CSS string into an item body

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ customPandoc
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ customPandoc
            >>= loadAndApplyTemplate postTemplate postsCtx
            >>= loadAndApplyTemplate defTemplate postsCtx
            >>= relativizeUrls

    match "drafts/*.md" $ do
        route $ setExtension "html"
        compile $ customPandoc
            >>= loadAndApplyTemplate postTemplate postsCtx
            >>= loadAndApplyTemplate defTemplate postsCtx
            >>= relativizeUrls

    mkListPage "archive.html" "posts/*" "posts" "Archives" "templates/archive.html"
    mkListPage "drafts.html" "drafts/*" "posts" "Drafts" "templates/drafts.html"

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postsCtx (return posts) <>
    --                 constField "title" "Archives"            <>
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postsCtx (pure posts) <>
                    notPost <>
                    defaultContext <>
                    titleField "title"

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration -- removed custom deploy for CircleCI

writerConfig :: WriterOptions
writerConfig = def { writerExtensions = customExts }

readerConfig :: ReaderOptions
readerConfig = def { readerExtensions = customExts }

customPandoc :: Compiler (Item String)
customPandoc = pandocCompilerWith readerConfig writerConfig

customExts :: Extensions -- pandoc options
customExts = pandocExtensions 
        `mappend` extensionsFromList [ Ext_native_divs
                                     , Ext_literate_haskell
                                     , Ext_emoji
                                     , Ext_inline_code_attributes
                                     , Ext_inline_notes
                                     ]

-- for debugging
traceComp :: Show t => t -> Compiler t
traceComp x = unsafeCompiler (liftM2 (>>) print pure x)

mkListPage :: Identifier -> Pattern -> String -> String -> Identifier -> Rules ()
mkListPage ident sourceDir lstField pageName template = do
    create [ident] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll sourceDir
            let ctx =
                    listField lstField postsCtx (return posts) <>
                    notPost <>
                    constField "title" pageName <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate template ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

postTemplate :: Identifier
defTemplate  :: Identifier
postTemplate = "templates/post.html"
defTemplate = "templates/default.html"

notPost :: Context a
notPost = boolField "notPost" (const True)
