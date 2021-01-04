{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Contexts where

import Data.Time
import Data.Time.Locale.Compat
import Hakyll
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Applicative
import System.FilePath
import Debug.Trace


showtrace :: Show a => a -> a
showtrace = show >>= trace

postsCtx :: Context String
postsCtx =  lastField
         <> keywordField
        -- <> dateField "published" "%Y-%m-%d"
         <> defaultContext 


fixDates :: String -> String
fixDates = let replace c d = map (\x -> if x == c then d else x) in 
                                 intercalate "-" . reverse . words . replace '-' ' '
    -- since the hakyll date compiler is wonky
    -- using words instead of splitAt to keep from pulling in Data.List.Split

mkKeywords :: String -> String
mkKeywords = intercalate ", " . map head . group . sort . words

lastField :: Context String 
lastField = dateField' "last" "%Y-%m-%d"
-- lastField :: Context String 
-- lastField = maybeField fixDates "last"
    -- mapContext fixDates . field "last" $ \item -> do
    -- lst <- getMetadataField (itemIdentifier item) "last"
    -- pure $ fromMaybe empty lst

-- this should really be a builtin function
keywordField :: Context String
keywordField = maybeField mkKeywords "keywords"
    -- mapContext mkKeywords $ field "keywords" $ \item -> do
    -- keys <- getMetadataField (itemIdentifier item) "keywords"
    -- pure $ fromMaybe empty keys

-- map a function inside a value only if it exists
maybeField :: (String -> String) -> String -> Context a
maybeField f key = mapContext f . field key $ \item -> do
    val <- getMetadataField (itemIdentifier item) key
    pure $ fromMaybe empty val

-- taken from Hakyll.Web.Template.Context
-- modifying to allow for custom date field
dateField' :: --TimeLocale -- ^ Output time locale
                  String     -- ^ Key of date field (taken from and stored in this key)
               -> String     -- ^ Format to use on date
               -> Context a  -- ^ Resulting context
dateField' key fmt = let df = defaultTimeLocale in 
    field key $ \i -> do
        time <- getItemUTC' df key $ itemIdentifier i 
        pure $ formatTime df fmt time

getItemUTC' :: (MonadMetadata m, MonadFail m)
           => TimeLocale        -- ^ Output time locale
           -> String            -- ^ Key of date field
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime

getItemUTC' locale name id' = do
    metadata <- getMetadata id'
    let strVal = lookupString name metadata
        tryField fmt = strVal >>= parseTime' fmt 
        paths          = splitDirectories $ (dropExtension . toFilePath) id'

    maybe empty' pure . foldr1 (<|>) . map tryField $ formats
        -- [tryField fmt | fmt <- formats] 
        --
        -- [tryField "published" fmt | fmt <- formats] <>
        -- [tryField "date"      fmt | fmt <- formats] <>
        -- [parseTime' "%Y-%m-%d" . intercalate "-" . take 3 $ splitAll "-" fnCand | fnCand <- reverse paths] <>
        -- [parseTime' "%Y-%m-%d" . intercalate "-" $ fnCand | fnCand <- map (take 3) $ reverse . tails $ paths]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC': " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTimeM True locale
    formats =  [ "%a, %d %b %Y %H:%M:%S %Z"
            --    , "%Y-%m-%d"
               , "%a, %d %b %Y %H:%M:%S"
               , "%Y-%m-%dT%H:%M:%S%Z"
               , "%Y-%m-%dT%H:%M:%S"
               , "%Y-%m-%d %H:%M:%S%Z"
               , "%Y-%m-%d %H:%M:%S"
               , "%B %e, %Y %l:%M %p"
               , "%B %e, %Y"
               , "%b %d, %Y"
               ]