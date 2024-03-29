--------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes, BlockArguments, OverloadedStrings, ViewPatterns, PatternGuards, PatternSynonyms, DataKinds, ImportQualifiedPost, TupleSections #-}
module Main where

import Hakyll
import Hakyll.Core.Configuration
import Hakyll.Core.Identifier.Pattern
import Text.Pandoc
import Text.Pandoc.Extensions
import Data.Text qualified as T
import Data.Functor ((<&>))
import Data.Char (isUpper, isLower)
import Data.Tuple.Curry
import Control.Monad
import Data.Map qualified as Map
import Control.Applicative
import Control.Lens (view, set, over, (^.), (%~), (.~))
import Hakyll.Core.Logger qualified as L
import Text.Printf (printf)
import GHC.IO.Encoding qualified as Enc
import System.FilePath.Lens
import Control.Lens.Operators
import Contexts
import Utils
import Sass
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Pandoc.Shared (camelCaseStrToHyphenated)
import Data.Maybe (catMaybes)
import Series (buildSeries, seriesField)
import qualified GHC.IO.Encoding as Enc
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- to avoid invalid byte sequence error
  Enc.setLocaleEncoding Enc.utf8

  baseCtx <- buildBaseCtx
  hakyllWith config do

    seriesTags <- buildSeries "posts/*" (fromCapture "series/*.html")
    let postsCtx' = seriesField seriesTags <> postsCtx <> baseCtx 

    tagsRules seriesTags \tag pat -> do
        route idRoute
        compile do
            posts <- chronological =<< loadAll pat
            let fmtted = unwords $ map capitalize $ splitAll "_" tag
                title = printf "index: series (%s)" fmtted
                ctx = constField "title" title
                    <> constField "seriesName" fmtted
                    <> listField "posts" postsCtx' (pure posts)
                    <> baseCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/series.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

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

    match "siteroot/*.md" do
        route moveToRoot
        compile
            $ initialTransforms
            >>= customPandoc
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls'

    match "posts/*.md" do
        buildPost postsCtx'

    match "drafts/*.md" $ buildPost postsCtx'

    withMatches "**/*.lhs" \idents -> do
        -- prepend date and convert camelCase -> kebab-case because haskell module names are really particular
        metas <- Map.fromList <$> traverse (\idt -> (idt, ) <$> getMetadata idt) idents
        (`buildPostWithRoute` postsCtx') $ customRoute \ident -> 
            let Just date = lookupString "published" =<< metas Map.!? ident
             in toFilePath ident
                & over basename ((date <>) . ('-' :) . camelCaseStrToHyphenated)
                & extension .~ ".html"

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
                           <> boolField "index" (const True) -- hack to default to regular title
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
writerConfig = def { writerExtensions = exts', writerTableOfContents = True}
    where exts' = disableExtension Ext_literate_haskell customExts

readerConfig :: ReaderOptions
readerConfig = def { readerExtensions = customExts, readerStripComments = True }
    -- where exts' = customExts <> extensionsFromList [Ext_literate_haskell]

initialTransforms :: Compiler (Item String)
initialTransforms = getResourceBody >>= applyAsTemplate funcFields

customPandoc :: Item String -> Compiler (Item String)
customPandoc = renderPandocWithTransform readerConfig writerConfig walkPandocAST
    -- my contribution to Hakyll :DDDD

customExts :: Extensions -- pandoc options
customExts = pandocExtensions <> extensionsFromList
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
buildPost = buildPostWithRoute (setExtension "html")

buildPostWithRoute :: Routes -> Context String -> Rules ()
buildPostWithRoute router postsCtx' = do
    route router
    compile do
        -- ident <- getUnderlying
        -- seriesMeta <- getMetadataField ident "series"
        
        getResourceBody
            >>= customPandoc
            >>= loadAndApplyTemplate postTemplate postsCtx'
            >>= loadAndApplyTemplate defTemplate postsCtx'
            >>= relativizeUrls'

withMatches :: Pattern -> ([Identifier] -> Rules ()) -> Rules ()
withMatches pat f = match pat $ getMatches pat >>= f
