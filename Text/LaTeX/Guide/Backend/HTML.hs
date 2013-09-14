
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Guide.Backend.HTML (
  backend
  ) where

import Text.LaTeX.Guide.Syntax
import Text.LaTeX.Guide.Info hiding (LaTeX)
import Text.LaTeX.Guide.Auto
--
import Text.Blaze.Html5 (Html,toHtml,preEscapedToHtml,preEscapedToValue,(!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy.IO
--
import Data.Monoid
import Control.Monad.Trans.State
import Control.Applicative ((<$>))
import Data.List (intersperse)

resURL :: Text -> Text
resURL t = "https://raw.github.com/Daniel-Diaz/hatex-guide/master/res/" <> t

sectFromInt :: Int -> Html -> Html
sectFromInt 1 = H.h1
sectFromInt 2 = H.h2
sectFromInt 3 = H.h3
sectFromInt 4 = H.h4
sectFromInt 5 = H.h5
sectFromInt 6 = H.h6
sectFromInt n = error $ "Subsection with hierarchy of " ++ show n ++ " is not available in the HTML backend."

type SectionNumber = [Int]

showSN :: SectionNumber -> String
showSN = mconcat . intersperse "." . fmap show

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f [x] = [f x]
mapLast f (x:xs) = x : mapLast f xs

upgradeSN :: Int -> SectionNumber -> SectionNumber
upgradeSN n [] = replicate (n-1) 0 ++ [1]
upgradeSN n sn = mapLast (+1) $ take n $ sn ++ repeat 0

sectiondots :: Int -> Html
sectiondots 1 = mempty
sectiondots n = toHtml $ (mconcat $ replicate (n-1) str) ++ " "
  where
    str :: String
    str = ".........."

data HtmlState =
  HState { fnIndex :: Int -- Footnote index
         , sectionNumber :: SectionNumber -- Section numbering
         , htmlBody :: Html -- HTML body
         , fnHtml :: Html -- Footnotes html
         , tocHtml :: Html -- Table Of Contents
           }

defaultState :: HtmlState
defaultState = HState 1 [] mempty mempty mempty

type HtmlM = State HtmlState

ihtml :: Html -> HtmlM ()
ihtml h = modify $ \s -> s { htmlBody = htmlBody s <> h }

ihtmlf :: (Html -> Html) -> HtmlM () -> HtmlM ()
ihtmlf f hm = do
  s0 <- get
  let s1 = execState hm $ s0 { htmlBody = mempty }
  put $ s1 { htmlBody = htmlBody s0 <> f (htmlBody s1) }

execHtmlM :: HtmlM a -> Html
execHtmlM hm =
  do let s = execState hm defaultState
     H.h1 "Table of contents"
     tocHtml s
     H.br
     H.a ! A.href "#footnotes" $ "Footnotes"
     H.hr
     htmlBody s
     H.hr
     H.a ! A.id "footnotes" $ H.h1 "Footnotes"
     fnHtml s

htmlSyntax :: Syntax -> HtmlM ()
htmlSyntax (Raw t) = ihtml $ toHtml t
htmlSyntax (Section n s) = do
  t <- sectionNumber <$> get
  let t' = upgradeSN n t
      a  = "s" ++ showSN t'
  ihtmlf (sectFromInt n . (H.a ! A.id (preEscapedToValue a))) $ do
    ihtml $ toHtml $ (++". ") $ showSN t'
    htmlSyntax s
  let fortoc = do
        sectiondots n
        H.a ! A.href (preEscapedToValue $ '#' : a) $ do
          toHtml $ showSN t'
          toHtml (". " :: String)
          htmlBody $ execState (htmlSyntax s) defaultState
  modify $ \s -> s { sectionNumber = t'
                   , tocHtml = tocHtml s <> H.br <> fortoc
                     }
htmlSyntax (Bold s) = ihtmlf H.b $ htmlSyntax s
htmlSyntax (Italic s) = ihtmlf H.i $ htmlSyntax s
htmlSyntax (Code b t) =
  let f = if b then H.code else H.pre
  in  ihtml $ f $ preEscapedToHtml t
htmlSyntax (URL t) = ihtml $ H.a ! A.href (preEscapedToValue t) $ toHtml t
htmlSyntax (IMG t) = ihtml $ H.img ! A.src (preEscapedToValue $ resURL t) ! A.width "50%"
htmlSyntax LaTeX = ihtml $ H.i "LaTeX"
htmlSyntax HaTeX = ihtml $ H.i "HaTeX"
htmlSyntax (Math t) = ihtml $ H.i $ toHtml t
htmlSyntax (Append s1 s2) = htmlSyntax s1 >> htmlSyntax s2
htmlSyntax Empty = return ()
htmlSyntax (Footnote x) = do
  s <- get
  let i = fnIndex s
      str = '[' : (show i ++ "]")
      s'  = execState (htmlSyntax x) $ s { htmlBody = mempty }
      fn = H.p $ do H.a ! A.id (preEscapedToValue $ 'f' : show i) $ toHtml str
                    toHtml (" - " :: String)
                    htmlBody s'
      h  = H.a ! A.href (preEscapedToValue $ "#f" ++ show i) $ toHtml str
  modify $ \s -> s { fnIndex = fnIndex s + 1
                   , fnHtml = fnHtml s <> fn
                   , htmlBody = htmlBody s <> h
                     }

createManual :: IO Html
createManual = fmap (execHtmlM . sequence_ . fmap htmlSyntax) parseSections

backend :: IO ()
backend = do
  Prelude.putStrLn "Creating guide..."
  m <- createManual
  Prelude.putStrLn "Writing guide file..."
  let fp = outputName ".html"
  Data.Text.Lazy.IO.writeFile fp $ renderHtml m
  Prelude.putStrLn $ "Guide written in " <> fp <> "."
