module Series where

import Data.Function
-- inlined from hakyll-series::Hakyll.Web.Series since that package hasn't been updated in 6 years
import Hakyll.Core.Rules
import Hakyll.Core.Compiler
import Hakyll.Core.Metadata
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Web.Template.Context
import Hakyll 
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List (elemIndex, tails, scanl', find)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set qualified as Set
import Data.Maybe (isJust)
import Clay (itemid)

-- | Gets the series from an identifier. Similar to 'getTags',
-- except it only accepts one series per identifier.
getSeries :: MonadMetadata m => Identifier -> m (Maybe String)
getSeries = flip getMetadataField "series"

-- toAlt :: (Foldable f, Alternative m) => f a -> m a
toAlt :: Alternative f => Maybe a -> f a
toAlt = maybe empty pure

ensure :: Alternative f => (t -> Bool) -> t -> f t
ensure f x = x <$ guard (f x) 

infixr 1 >->
(>->) :: Functor f =>  (a -> f b) -> (b -> c) -> a -> f c
f >-> g = f >>> fmap g

-- | Generates four fields:
--
--    [@series@] The name of the series
--
--    [@seriesLength@] The total number of posts in the series
--
--    [@seriesCurPos@] The position of the current post in the series
--
--    [@seriesUrl@] The URL of the series page

seriesField :: Tags -> Context a
seriesField tags = Context $ const . \case
        "series"   
          -> seriesName
          >-> StringField

        "seriesCurPos" 
          -> itemIdentifier &&& otherPostsInSeries
          >>> sequence
          >=> ensure (notSingleton . snd)
          >>> fmap (uncurry elemIndex)
          >=> toAlt
          -- >=> ensure (> 0)
          >-> succ
          >>> show
          >>> StringField
    
        "seriesPrev" 
          -> getAt reverse
        
        "seriesNext"
          -> getAt id

        "seriesLength" 
          -> otherPostsInSeries
          >-> length
          >>> show
          >>> StringField

        "seriesUrl"    
          -> seriesName
          >=> tagsMakeId tags
          >>> getRoute
          >=> toAlt
          >-> toUrl
          >>> StringField
    
        _ -> const empty
  where
    seriesName = itemIdentifier
             >>> getSeries
             >=> toAlt
    otherPostsInSeries = seriesName
                     >=> flip lookup (tagsMap tags)
                     >>> toAlt
    getAt f item = do
      let ident = itemIdentifier item
      posts <- f <$> otherPostsInSeries item
      (_ : post : _) <- toAlt $ find (([ident] ==) . take 1) $ tails posts
      route <- getRoute post
      StringField . toUrl <$> toAlt route
    
    notSingleton = not . null . drop 1

-- | Similar to the 'buildTags' function in "Hakyll.Web.Tags", except
-- checks the series field, and can only accept one series per item.
buildSeries :: (MonadFail m, MonadMetadata m)
            => Pattern
            -> (String -> Identifier) -- ^ Function for converting a given series name into an identifier for its page
            -> m Tags
buildSeries pattrn makeId = do
    ids <- getMatches pattrn
    tagMap <- foldM addTags Map.empty ids
    let set' = Set.fromList ids
    inOrder <- (traverse.traverse) sortChronological (Map.assocs tagMap)
    pure $ Tags inOrder makeId (PatternDependency pattrn set')
  where
    addTags tagMap id' =
        maybe tagMap (\k -> Map.insertWith (++) k [id'] tagMap) <$> getSeries id'