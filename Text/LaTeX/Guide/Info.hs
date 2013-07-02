
module Text.LaTeX.Guide.Info (sectionList,contributors,Backend(..),parseSections,outputName) where

import Text.LaTeX.Guide.Syntax
import System.FilePath
import Data.Monoid

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
contributors = [ ]

-- | Available backends.
data Backend = LaTeX | Wiki

parseSections :: IO [Syntax]
parseSections = mapM (parseFile . (<.> "htxg") . combine "src") sectionList

outputName :: String -> FilePath
outputName = mappend "hatex-guide"
