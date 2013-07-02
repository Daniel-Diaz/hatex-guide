
module Text.LaTeX.Guide (
   -- * Backends
   Backend (..)
 , writeGuide
   -- * Info
 , sectionList
 , contributors
 ) where

import Text.LaTeX.Guide.Info
import qualified Text.LaTeX.Guide.Backend.LaTeX as LaTeX
import qualified Text.LaTeX.Guide.Backend.Wiki  as Wiki

-- | Write in the current directory the LaTeX User's Guide using
--   a determined backend.
writeGuide :: Backend -> IO ()
writeGuide LaTeX = LaTeX.backend
writeGuide  Wiki =  Wiki.backend
