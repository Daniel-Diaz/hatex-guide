
import System.Directory
  ( copyFile
  , getAppUserDataDirectory
  , createDirectoryIfMissing
    )
import Text.LaTeX.Guide.Update
import System.FilePath ((</>),(<.>))
import Control.Applicative ((<$>))
-- Cabal
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Verbosity (normal)

main :: IO ()
main = do
  -- Update (create if missing) guide files
  updateGuide "."
  -- Write aux module
  let dg = "Text" </> "LaTeX" </> "Guide"
  createDirectoryIfMissing True dg
  getAux >>= writeFile (dg </> "Auto" <.> "hs") . auxmodule
  --
  defaultMain

data Aux = Aux { guideVersion :: Version }

getAux :: IO Aux
getAux = do
  pd <- packageDescription <$> readGenericPackageDescription normal "hatex-guide.cabal"
  return $ Aux (pkgVersion $ package pd)

auxmodule :: Aux -> String
auxmodule a = unlines [
   "-- | Automatically generated module."
 , "module Text.LaTeX.Guide.Auto ("
 , "  guideVersion"
 , "    ) where"
 , ""
 , "import Data.Version"
 , ""
 , "-- | The version of the guide. Based on the version of the package."
 , "guideVersion :: Version"
 , "guideVersion = " ++ (let v = guideVersion a
                         in  "makeVersion " ++ show (versionNumbers v))
   ]
