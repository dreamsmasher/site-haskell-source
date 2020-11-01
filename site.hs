--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Core.Configuration
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Pandoc
import           Text.Pandoc.Extensions
import           Data.List
import           Control.Monad
import qualified Debug.Trace as T
import           Control.Applicative (Alternative (..))
import           Data.Maybe

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

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
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- (take 5 <$>) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext <>
                    titleField "title"

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =  keywordField
        <> mapContext fixDates (dateField "last" "%d-%m-%Y")
        <> dateField "published" "%Y-%m-%d"
        <> defaultContext 

config :: Configuration
config = defaultConfiguration -- removed custom deploy for CircleCI

customExts :: Extensions -- pandoc options
customExts = 
    pandocExtensions 
        `mappend` extensionsFromList [ Ext_native_divs
                                     , Ext_literate_haskell
                                     , Ext_emoji
                                     , Ext_inline_code_attributes
                                     , Ext_inline_notes
                                     ]

writerConfig :: WriterOptions
writerConfig = def { writerExtensions = customExts }

readerConfig :: ReaderOptions
readerConfig = def { readerExtensions = customExts }

customPandoc :: Compiler (Item String)
customPandoc = pandocCompilerWith readerConfig writerConfig

fixDates :: String -> String
fixDates = let replace c d = map (\x -> if x == c then d else x) in 
                                 intercalate "-" . reverse . words . replace '-' ' '
    -- since the hakyll date compiler is wonky
    -- using words instead of splitAt to keep from pulling in Data.List.Split

mkKeywords :: String -> String
mkKeywords = intercalate ", " . map head . group . sort . words

-- this should really be a builtin function
keywordField :: Context String
keywordField = mapContext mkKeywords $ field "keywords" $ \item -> do
    keys <- getMetadataField (itemIdentifier item) "keywords"
    pure $ fromMaybe empty keys

-- map a function inside a value only if it exists
maybeField :: String -> (String -> String) -> (Item a -> Maybe (Compiler String)) -> Context a
maybeField key f g = mapContext f . field key $ fromMaybe empty . g


