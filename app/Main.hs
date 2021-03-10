--------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes, BlockArguments, OverloadedStrings, LambdaCase #-}
module Main where

import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Core.Configuration
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Pandoc
import           Text.Pandoc.Extensions
import           Data.List
import qualified Data.Text as T
import           Data.Maybe
import           Data.Char (isUpper, isLower)
import           Data.Tuple.Curry
import           Control.Monad
import           Control.Applicative
import           Control.Lens (view, set, over, (^.), (%~), (.~))
import           Control.Lens.Tuple
import           Data.Foldable
import qualified Hakyll.Core.Logger as L
import           Control.Monad.IO.Class
import           Text.Pandoc.Walk
import           CSS
import           Contexts
--------------------------------------------------------------------------------

main :: IO ()
main = do
  baseCtx <- buildBaseCtx
  let postsCtx' = postsCtx <> baseCtx
      buildPost = do
        route $ setExtension "html"
        compile $ customPandoc
          >>= loadAndApplyTemplate postTemplate postsCtx'
          >>= loadAndApplyTemplate defTemplate postsCtx'
          >>= relativizeUrls'

  hakyllWith config do
    match (fromList ["CNAME", "site.webmanifest"]) do
        route idRoute
        compile copyFileCompiler 

    match "images/**" do
        route   idRoute
        compile copyFileCompiler

    match "css/*" do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) do
        route   $ setExtension "html"
        compile $ customPandoc
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls'

    match "posts/*" buildPost
    match "drafts/*.md" buildPost

    mapM_ (\t -> uncurryN mkListPage t postsCtx' [baseCtx])
        [ ("archive.html", "posts/*", "posts", "Archives", "templates/archive.html") 
        , ("drafts.html", "drafts/*", "posts", "Drafts", "templates/drafts.html")
        ]
    --mkListPage "archive.html" "posts/*" "posts" "Archives" "templates/archive.html" [baseCtx]
    -- mkListPage "drafts.html" "drafts/*" "posts" "Drafts" "templates/drafts.html" [baseCtx]

    match "index.html" do
        route idRoute
        compile do
            posts <- take maxIndexPagePosts <$> (recentFirst =<< loadAll "posts/*")
            let indexCtx = listField "posts" postsCtx' (pure posts) 
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

customPandoc :: Compiler (Item String)
customPandoc = pandocCompilerWithTransform readerConfig writerConfig transformInlineCode

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

mkListPage :: Identifier 
           -> Pattern
           -> String
           -> String
           -> Identifier
           -> Context String -- pass in the result of generating a post context thru IO
           -> [Context String] -- list of all other contexts to concatenate with
           -> Rules ()
mkListPage ident sourceDir lstField pageName template postCtx ctxs = do
    create [ident] do
        route idRoute
        compile do
            posts <- recentFirst =<< loadAll sourceDir
            let ctx =
                    listField lstField postCtx (pure posts) <>
                    notPost <>
                    constField "title" pageName <>
                    (if null ctxs then defaultContext else mconcat ctxs)

            makeItem ""
                >>= loadAndApplyTemplate template ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls'

postTemplate :: Identifier
defTemplate  :: Identifier
postTemplate = "templates/post.html"
defTemplate = "templates/default.html"

notPost :: Context a
notPost = boolField "notPost" (const True)

transformInlineCode :: Pandoc -> Pandoc
transformInlineCode = walk transform
    where addClass :: T.Text -> Attr -> Attr
          addClass klass = over _2 (klass :)
          syms = S.fromList "<>{}()?+-/*=!@#$%^&|._" 
          isInlineType = maybe False (isUpper . fst) . T.uncons
          isInlineOp = T.all (`S.member` syms)
          isModule = isJust . T.find (== '.')
          transform = \case
            c@(Code a t) | isInlineType t -> Code (addClass (if isModule t then "inline-mod" else "inline-type") a) t
                         | isInlineOp t -> Code (addClass "inline-op" a) t
                         | otherwise -> c
            e -> e