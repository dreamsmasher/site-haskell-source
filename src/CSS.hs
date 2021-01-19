{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CSS (runSiteCSS, renderSiteCSS) where

import Clay as C
import qualified Clay.Elements as E
import qualified Clay.Flexbox as F
import qualified Clay.Media as M
import qualified Clay.Text as CT
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as IOT
import Clay.Selector
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Semigroup
import Data.Tuple.Curry
import Prelude hiding (span, rem, (**))

-- this is its own program, so that we conditionally recompile as needed

-- small helpers because CSS is wack
apply4 :: (a -> a -> a -> a -> b) -> a -> b
apply4 = join . join . join

appXXZZ :: (a -> a -> b -> b -> c) -> a -> b -> c
appXXZZ f a b = f a a b b

marginZA :: Css
marginZA = margin nil nil auto auto

noMargin :: Css
noMargin = apply4 margin nil
-- 

runSiteCSS :: IO ()
runSiteCSS = putStrLn renderSiteCSS

renderSiteCSS :: String
renderSiteCSS = T.unpack . render $ do
  -- CSS could honestly just be a monoid...
  siteHtml >> siteBody >> logo >> exps >> listInd
  siteHr >> siteNav >> refs >> tables >> tableHead >> pic >> pageCont 
  siteContent >> siteFooter >> siteH1 >> siteH2 >> CSS.name
  -- code block stuff
  divSCEmph >> divSCBefr
  sourceLine >> sourceCode >> siteCode >> sitePre >> highlight
  siteMedia >> codeMedia 

siteHtml :: Css
siteHtml =
  html ? do
    fontFamily ["Space Mono"] [monospace]
    height $ vh 100
    fontSize $ em 1.4

siteBody :: Css
siteBody =
  body ? do
    textRendering optimizeLegibility
    noMargin
    display block
    minHeight $ pct 99
    fontVariantLigatures
    backgroundColor "#121111"
    color white
    position relative
    textIndent $ indent (em 1)

listInd :: Css
listInd = li ? color yellow

siteHr :: Css
siteHr = hr ? border dashed (px 3) (rgb 0 170 170)

siteNav :: Css
siteNav =
  nav ? do
    textAlign center
    textIndent $ indent nil
    display block
    ".delim" & do
      color white
      fontSize (rem 2)
      marginZA
    a ? do
      fontSize (rem 1.8)
      fontWeight bold
      color white
      textDecoration none
      textTransform uppercase

-- refs :: Css
refs :: Css
refs =
  a ? do
    link & color magenta
    visited & color magenta
    hover & (color white >> textTransform uppercase)
    active & color yellow

appAll :: [Selector] -> Css -> Css
appAll xs f = traverse_ (? f) xs

tables :: Css
tables = appAll [table, th, td] $ do
  border dashed (px 1) "#ffff55"
  marginZA
  minWidth (pct 60)
  borderCollapse collapse
  maxWidth (pct 100)

tableHead :: Css
tableHead =
  thead ? do
    verticalAlign middle
    borderColor inherit
    color cyan

exps :: Css
exps = appAll [sup, sub] $ do
  verticalAlign vAlignBaseline
  position relative
  top $ rem (-0.4)
  fontSize (rem 0.7)

pic :: Css
pic =
  ".pic" ? do
    display block
    maxWidth (pct 80)
    height auto
    marginZA

pageCont :: Css
pageCont =
  "#page-container" ? do
    position relative
    width (pct 100)
    display flex
    flexDirection column
    flexGrow 1
    F.flex 1 1 auto
    paddingBottom nil

-- uncharted territory (non-typesafe functions) --
fontVariantLigatures :: Css
fontVariantLigatures = "font-variant-ligatures" -: "no-common-ligatures"

gridTemplateRows :: Css
gridTemplateRows = "grid-template-rows" -: "auto 1fr auto"

-- userSelects :: Css
-- userSelects = traverse_ (-: "none") [ "-webkit-touch-callout"
--                                     , "-webkit-user-select"
--                                     , "-khtml-user-select"
--                                     , "-moz-user-select"
--                                     , "-ms-user-select"
--                                     , "user-select"
--                                     ]
 
-- 
siteContent :: Css
siteContent = "#content" ? paddingBottom (rem 2.4)

siteFooter :: Css
siteFooter =
  footer ? do
    fontSize (em 0.8)
    flexShrink 0
    width (pct 100)
    position relative
    bottom nil

siteH1, siteH2, name :: Css
siteH1 = h1 ? fontSize (em 2) >> color "#ffff55" >> textIndent (indent nil)
siteH2 = h2 ? fontSize (rem 1.6)
name = ".name" ? color red

-- numbers indicate pixel offset for my kewl CGA effect
mkLogo :: (Double, Double, Double) -> Css
mkLogo (m, y, c) = traverse_ (uncurryN textShadow) logoColors
    where logoColors = [ (px m, px m, 0, magenta)
                       , (px y, px y, 0, yellow)
                       , (px c, px c, 0, cyan)
                       ]
logo :: Css
logo =
  ".logo" ? do
    fontWeight bold
    display inlineBlock
    fontSize $ em 2.4
    color white
    mkLogo (1, 1.5, -0.8)
    textDecoration none
    position relative
    marginTop (rem 1)
    textIndent $ indent nil

-- media queries

siteMedia :: Css
siteMedia = smoller >> smol >> med >> lrg

smoller :: Css
smoller = query M.screen [M.maxWidth (px 319)] $ do
  body ? (noMargin >> appXXZZ padding nil (pct 5))
  header ? appXXZZ margin (rem 4.2) nil
  let txC = textAlign center
  nav ? do
    margin nil auto auto (rem 3) >> txC
    a ? (display block >> lineHeight (unitless 1.6))
  footer ? txC
  ".logo" ? do
    txC >> margin (rem 1) auto auto (rem 3) >> color white >> fontSize (rem 1.6)
    a ? fontSize (rem 2.4)

smol :: Css
smol = query M.screen [M.minWidth (px 320)] $ do
  body ? (noMargin >> appXXZZ padding nil (pct 2))
  footer ? textAlign (alignSide sideRight)
  ".logo" ? do
    display block >> textAlign center >> marginZA
    a ? (fontSize (rem 4.8) >> mkLogo (2.4, 3.6, -1.6))
  nav ** a ? display inlineBlock

lrg :: Css
lrg = query M.screen [M.minWidth (px 640)] $ do
  body ? (appXXZZ margin (px 2) auto >> lineHeight (em 1.6))
  nav ? do
    noMargin >> textAlign (alignSide sideRight)
    a ? display inline
  ".logo" ? textAlign (alignSide sideLeft) >> a ? (float floatLeft >> fontSize (rem 1.8))

med :: Css
med = queryOnly M.screen [M.maxDeviceWidth (px 480)] $ do
  html ? fontSize (vh 2.8)
  header ? do
    marginTop $ em 0.8
    marginBottom $ em (-0.8)
    width $ pct 100
    maxHeight $ pct 30
  nav ? do
    display block
    textAlign center
    position relative
    margin (em 1) auto (rem 2.4) auto

    a ? fontSize (rem 0.6) >> marginZA
    ".delim" & fontSize (rem 0.6)

  body ? do
    fontSize $ rem 0.6
    maxWidth (vw 100)
    lineHeight (pct 136) -- i forgot why i did this
    apply4 margin $ em 0.2

  h1 ? do
    fontSize $ rem 1
    appXXZZ margin (rem 0.4) auto
    lineHeight $ pct 120

  h2 ? fontSize (em 0.8)
  hr ? margin (em (-3.0)) auto auto (em 1.6)

--

-- code block stuff

sourceLine :: Css
sourceLine =
  a `with` "sourceLine" ? do
    display inlineBlock
    lineHeight (unitless 1.25)
    pointerEvents none
    color inherit
    textDecoration inherit
    ":empty" & height (em 1.2)

divSC :: Selector
divSC = C.div # "sourceCode"

divSCEmph, divSCBefr :: Css
divSCEmph = divSC |+ p |> E.em ? fontSize (rem 0.8)
divSCBefr =
  divSC `with` before ? do
    content $ attrContent "data-caption"
    fontSize $ rem 0.7
    color yellow

sourceCode :: Css
sourceCode =
  ".sourceCode" ? do
    color white
    textIndent initial
    appXXZZ margin (em 1) nil
    lineHeight $ em 1.2
    overflow auto

siteCode :: Css
siteCode =
  code ? do
    fontFamily ["Fira Code"] [monospace]
    color cyan
    ".sourceCode" ? do
      whiteSpace CT.pre
      position relative
      fontSize $ pct 80

sitePre :: Css
sitePre =
  pre ? do
    noMargin
    apply4 padding (em 0.8)
    whiteSpace preWrap
    textAlign (alignSide sideLeft)
    border dashed (px 2) white

    ".numberSource" ? do
        borderLeft solid (px 1) "#aaaaaa" 
        paddingLeft (px 4)
    
        a `with` ".sourceLine" ? do
          position relative >> C.left (em (-4))

          before & do
            content $ attrContent "title"
            position relative
            C.left $ em (-1)
            textAlign (alignSide sideRight)
            verticalAlign vAlignBaseline
            borderStyle none
            pointerEvents allEvents
            display inlineBlock
            "-webkit-touch-callout" -: "none"
            userSelect none
            appXXZZ padding nil (px 4)
            color "#aaaaaa"


-- media stuff for code blocks
codeMedia :: Css
codeMedia = do
  let dummy = [M.minDeviceWidth $ px 0] -- match anything

  query M.screen dummy $ do
      display block
      a `with` ".sourceLine" ? before & textDecoration underline 

  query M.print dummy $ do
    code `with` ".sourceCode" ? display block
    a `with` "sourceLine" ? (textIndent (indent $ em (-1)) >> paddingLeft (em 1))
    
-- syntax highlighting

highlight :: Css
highlight = code ** span ? do
    -- direct translation from the CSS
    -- there's definitely a better way to do this
    -- for now, thank god for vim macros
    ".al" ? color "#00aaaa" >> fontWeight bold  -- Alert 
    ".an" ? color yellow >> fontWeight bold >> fontStyle italic  -- Annotation 
    ".at" ? color "#aa00aa"  -- Attribute 
    ".bn" ? color "#55ff55"  -- BaseN 
    -- ".bu" ?  - BuiltIn 
    ".cf" ? color "#ff55ff" >> fontWeight bold  -- ControlFlow 
    ".ch" ? color "#4070a0"  -- Char 
    ".cn" ? color "#880000"  -- Constant 
    ".co" ? color "#60a0b0" >> fontStyle italic  -- Comment 
    ".cv" ? color "#60a0b0" >> fontWeight bold >> fontStyle italic  -- CommentVar 
    ".do" ? color "#ba2121" >> fontStyle italic  -- Documentation 
    ".dt" ? color "#00aaaa"  -- DataType 
    ".dv" ? color "#00aaaa"  -- DecVal 
    ".er" ? color "#ff0000" >> fontWeight bold  -- Error 
    -- ".ex" ?  - Extension 
    ".fl" ? color "#55ff55"  -- Float 
    ".fu" ? color "#aa00aa"  -- Function 
    -- ".im" ?  - Import 
    ".in" ? color "#60a0b0" >> fontWeight bold >> fontStyle italic  -- Information 
    ".kw" ? color "#ff55ff" >> fontWeight bold  -- Keyword 
    ".op" ? color "#666666"  -- Operator 
    ".ot" ? color "#55ffff"  -- Other 
    ".pp" ? color "#ffffff"  -- Preprocessor 
    ".sc" ? color "#4070a0"  -- SpecialChar 
    ".ss" ? color "#bb6688"  -- SpecialString 
    ".st" ? color "#5555ff"  -- String 
    ".va" ? color "#19177c"  -- Variable 
    ".vs" ? color "#4070a0"  -- VerbatimString 
    ".wa" ? color "#60a0b0" >> fontWeight bold >> fontStyle italic  -- Warning 
