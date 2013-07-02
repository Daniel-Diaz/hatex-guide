
module Text.LaTeX.Guide (
   -- * Backends
   Backend (..)
 , writeBackend
   -- * Info
 , sectionList
 , contributors
 ) where

import Text.LaTeX.Guide.Info
import qualified Text.LaTeX.Guide.Backend.LaTeX as LaTeX
import qualified Text.LaTeX.Guide.Backend.Wiki  as Wiki

-- | Write in the current directory the LaTeX User's Guide using
--   a determined backend.
writeBackend :: Backend -> IO ()
writeBackend LaTeX = LaTeX.backend
writeBackend  Wiki =  Wiki.backend
