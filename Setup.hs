
import Distribution.Simple
import System.Directory
  ( copyFile
  , getAppUserDataDirectory
  , createDirectoryIfMissing
    )
import Text.LaTeX.Guide (sectionList)
import Text.LaTeX.Guide.Info (otherResources)
import System.FilePath ((</>),(<.>))

main :: IO ()
main = do
  d <-  getAppUserDataDirectory "hatex-guide"
  createDirectoryIfMissing True $ d </> "src"
  createDirectoryIfMissing True $ d </> "res"
  mapM_ (\s -> let fp = "src" </> s <.> "htxg"
               in  copyFile fp $ d </> fp) sectionList
  mapM_ (\r -> let fp = "res" </> r
               in  copyFile fp $ d </> fp) otherResources
  defaultMain
