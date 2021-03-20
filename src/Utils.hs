{-# LANGUAGE BlockArguments, OverloadedStrings, LambdaCase #-}
module Utils where
import Text.Pandoc.Walk
import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Hakyll
import Hakyll.Core.Configuration
import Control.Lens hiding (Context)
import Control.Monad
import Data.Char (isUpper, isLower)
import Data.Maybe

import Contexts

addClass :: T.Text -> Attr -> Attr
addClass klass = over _2 (klass :)

getClasses :: Attr -> [T.Text]
getClasses (_, c, _) = c

syms :: Set Char
syms = S.fromList "<>{}()?+-/*=!@#$%^&|._" 

-- we need to hook into the raw string before passing it into pandoc

-- TODO use parsec for this if more AST conversions get added
walkPandocAST :: Pandoc -> Pandoc
walkPandocAST = walk transform
    where 
          isInlineType = maybe False (isUpper . fst) . T.uncons
          isInlineOp = T.all (`S.member` syms)
          isModule = isJust . T.find (== '.')
          transform = \case
            c@(Code a t) 
                | isInlineType t -> Code (addClass (if isModule t then "inline-mod" else "inline-type") a) t
                | isInlineOp t -> Code (addClass "inline-op" a) t
                | otherwise -> c
            e -> e
        
moveToRoot :: Routes
moveToRoot = gsubRoute "siteroot/" (const "") `composeRoutes` setExtension "html"

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
            let ctx = mconcat 
                  [ listField lstField postCtx (pure posts) 
                  , notPost
                  , constField "title" pageName 
                  , if null ctxs then defaultContext else mconcat ctxs
                  ]

            makeItem ""
                >>= loadAndApplyTemplate template ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls'

