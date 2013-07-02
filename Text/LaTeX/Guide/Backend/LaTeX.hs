
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Guide.Backend.LaTeX (
 backend
  ) where

import Text.LaTeX.Guide.Syntax
import Text.LaTeX.Guide.Info hiding (LaTeX)
--
import Text.LaTeX
import Text.LaTeX.Packages.Color
import Text.LaTeX.Packages.Hyperref
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Babel
import Data.Version (showVersion,versionBranch)
import Data.Text (unpack)
import Data.Text.IO
import Data.List (intersperse)

sectFromInt :: Int -> LaTeX -> LaTeX
sectFromInt 1 = section
sectFromInt 2 = subsection
sectFromInt 3 = subsubsection
sectFromInt 4 = paragraph
sectFromInt 5 = subparagraph
sectFromInt n = error $ "Subsection with hierarchy of " ++ show n ++ " is not available in the LaTeX backend."

hatexSyntax :: Syntax -> LaTeX
hatexSyntax (Raw t) = raw $ protectText t
hatexSyntax (Section n s) = sectFromInt n $ hatexSyntax s
hatexSyntax (Bold s) = textbf $ hatexSyntax s
hatexSyntax (Italic s) = textit $ hatexSyntax s
hatexSyntax (Code b t) = let f = if b then texttt . raw . protectText
                                      else quote . verbatim
                             c = ModColor $ RGB255 50 50 255
                         in color c <> f t <> normalcolor
hatexSyntax (URL t) = let u = createURL $ unpack t
                      in  url u
hatexSyntax (IMG t) = center $ includegraphics [] $ unpack t
hatexSyntax LaTeX = latex
hatexSyntax HaTeX = hatex
hatexSyntax (Math t) = math $ raw t
hatexSyntax (Footnote s) = footnote $ hatexSyntax s
hatexSyntax (Append s1 s2) = hatexSyntax s1 <> hatexSyntax s2
hatexSyntax Empty = mempty

thePreamble :: LaTeX
thePreamble =
    documentclass [] article
 <> uselanguage English
 <> usepackage [utf8] inputenc
 <> usepackage [] hyperref
 <> usepackage [] graphicx
 <> usepackage [] pcolor
 <> title ("The " <> hatex <> " User's Guide")
 <> author (raw "Daniel D\\'iaz")
 <> linespread 1.4

theTitle :: LaTeX
theTitle = let xs = versionBranch version in flushright (
    resizebox (Pt 300) (Pt 40) (textbf "HATEX " <> (rendertex $ head xs)) <> lnbk
 <> textit ("The User's Guide (with version " <> fromString (showVersion version) <> ")") <> lnbk
 <> rule (Just $ Pt 10) (CustomMeasure textwidth) (Pt 2) <> lnbk
 <> url (createURL "https://github.com/Daniel-Diaz/HaTeX") <> lnbk
 <> textit (textbf "Main author: " <> raw "Daniel D\\'iaz (" <> texttt "dhelta.diaz@gmail.com" <> ")" ) <> lnbk
 <> if null contributors then mempty else
        ("Contributors:" <> lnbk <> mconcat (fmap ((<> lnbk) . fromString) contributors))
     )
 <> raw "\\vfill{}"
 <> flushleft ("Date of creation: " <> today <> ".")

initial :: LaTeX
initial = 
    raw "\\setcounter{page}{-1}" <> thispagestyle empty <> theTitle <> newpage
 <> thispagestyle empty <> tableofcontents <> newpage

createManual :: IO LaTeX
createManual = fmap (mappend thePreamble . document . mappend initial . mconcat . intersperse newpage . fmap hatexSyntax) parseSections

backend :: IO ()
backend = do 
  Prelude.putStrLn "Creating guide..."
  m <- createManual
  Prelude.putStrLn "Writing guide file..."
  let fp = outputName ".tex"
  Data.Text.IO.writeFile fp $ render m
  Prelude.putStrLn $ "Guide written in " <> fp <> "."
