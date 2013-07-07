
module Text.LaTeX.Guide.Update (
    updateGuide
  ) where

import System.Directory (
   getCurrentDirectory
 , setCurrentDirectory
 , createDirectoryIfMissing
 , getAppUserDataDirectory
 , copyFile
   )
import System.FilePath ((</>),(<.>))
import Text.LaTeX.Guide.Info (sectionList,otherResources)

-- | Update files in the user /hatex-guide/ directory, using
--   the files contained in a given 'FilePath'.
--
--   More in detail, @updateGuide fp@ sets the current directory
--   to @fp@, then it looks in the @src@ and @res@ directories
--   for the files specified by 'sectionList' and 'otherResources'
--   respectively. Then, it copies these files overwriting those
--   in the user /hatex-guide/ directory. This way, the next time
--   that 'writeGuide' is called it will use the updated files.
updateGuide :: FilePath -> IO ()
updateGuide fp = do
  d0 <- getCurrentDirectory
  setCurrentDirectory fp
  appd <- getAppUserDataDirectory "hatex-guide"
  putStrLn "Updating 'src'..."
  createDirectoryIfMissing True $ appd </> "src"
  mapM_ (\s -> let d = "src" </> s <.> "htxg"
               in  copyFile d $ appd </> d) sectionList
  putStrLn "Updating 'res'..."
  createDirectoryIfMissing True $ appd </> "res"
  mapM_ (\r -> let d = "res" </> r
               in  copyFile d $ appd </> d) otherResources
  setCurrentDirectory d0
