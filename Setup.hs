
import System.Directory
  ( copyFile
  , getAppUserDataDirectory
  , createDirectoryIfMissing
    )
import Text.LaTeX.Guide.Info (sectionList,otherResources)
import System.FilePath ((</>),(<.>))
import Control.Applicative ((<$>))
-- Cabal
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (normal)
-- Version
import Data.Version

main :: IO ()
main = do
  d <-  getAppUserDataDirectory "hatex-guide"
  createDirectoryIfMissing True $ d </> "src"
  createDirectoryIfMissing True $ d </> "res"
  mapM_ (\s -> let fp = "src" </> s <.> "htxg"
               in  copyFile fp $ d </> fp) sectionList
  mapM_ (\r -> let fp = "res" </> r
               in  copyFile fp $ d </> fp) otherResources
  -- Write aux module
  let dg = "Text" </> "LaTeX" </> "Guide"
  createDirectoryIfMissing True dg
  getAux >>= writeFile (dg </> "Aux" <.> "hs") . auxmodule
  --
  defaultMain

data Aux = Aux { guideVersion :: Version }

getAux :: IO Aux
getAux = do
  pd <- packageDescription <$> readPackageDescription normal "hatex-guide.cabal"
  return $ Aux (pkgVersion $ package pd)

auxmodule :: Aux -> String
auxmodule a = unlines [
   "module Text.LaTeX.Guide.Aux ("
 , "  guideVersion"
 , "    ) where"
 , ""
 , "import Data.Version"
 , ""
 , "-- | The version of the guide. Based on the version of the package."
 , "guideVersion :: Version"
 , "guideVersion = " ++ (let v = guideVersion a
                         in  "Version " ++ show (versionBranch v) ++ " []")
   ]
