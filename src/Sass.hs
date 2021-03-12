module Sass 
( sassFileCompiler
, sassBodyCompiler
, sassStrCompiler
, runSassFile
, runSassBody
, runSassStr
, sassRoute
)
where

import Text.Sass
import Hakyll

compileSass 
    :: (String -> SassOptions -> IO (Either SassError String))
    -> (String -> Compiler a)
    -> SassOptions
    -> String
    -> Compiler a
compileSass f act opts s = unsafeCompiler (f s opts)
    >>= either (fail . show) act

sassFileCompiler :: SassOptions -> FilePath -> Compiler (Item String)
sassFileCompiler = compileSass compileFile makeItem

sassBodyCompiler :: SassOptions -> Item String -> Compiler (Item String)
sassBodyCompiler opts = withItemBody $ compileSass compileString pure opts 

sassStrCompiler :: SassOptions -> String -> Compiler (Item String)
sassStrCompiler = compileSass compileString makeItem 

runSassFile :: FilePath -> Compiler (Item String)
runSassBody :: Item String -> Compiler (Item String)
runSassStr :: String -> Compiler (Item String)
runSassFile = sassFileCompiler def
runSassBody = sassBodyCompiler def
runSassStr = sassStrCompiler def

sassRoute :: Routes
sassRoute = setExtension ".css"