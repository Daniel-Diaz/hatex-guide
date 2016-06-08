
{-# LANGUAGE CPP #-}

module Text.LaTeX.Guide.Info (
   sectionList
 , contributors
 , Backend (..)
 , parseSections
 , outputName
 , otherResources) where

import Text.LaTeX.Guide.Syntax
import System.FilePath
import System.Directory (getAppUserDataDirectory)

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mappend)
#endif

-- | Ordered list of sections.
sectionList :: [String]
sectionList = [
   "preface"
 , "basics"
 , "monad"
 , "class"
 , "packages"
 , "epilogue"
 ]

-- | List of contributors. Please, insert your name here if you have contributed
--   in some way to the guide.
contributors :: [String]
contributors = [ "GetContented" ]

-- | Available backends.
data Backend = LaTeX | Wiki | HTML

parseSections :: IO [Syntax]
parseSections = do
  d <- getAppUserDataDirectory "hatex-guide"
  mapM (parseFile . (<.> "htxg") . combine d . combine "src") sectionList

outputName :: String -> FilePath
outputName = mappend "hatex-guide"

-- | Other resources (images).
--
--   Files stored under the /res/ directory.
otherResources :: [String]
otherResources = [ "machine.png" ]
