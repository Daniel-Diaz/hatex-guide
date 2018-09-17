
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Text.LaTeX.Guide.Backend.Wiki (
   backend
 ) where

import Text.LaTeX.Guide.Syntax
import Text.LaTeX.Guide.Info hiding (Backend(..))
import Data.Monoid
import Data.Text
import Data.Text.IO
import Data.Functor
import Data.Function
import Prelude (Eq (..), Num (..),IO,Monad (..), Int, Show (..))

#if MIN_VERSION_base(4,11,0)
import Prelude (Semigroup)
#endif

import Data.String (IsString (..))

tag :: Text -> Text -> Text
tag t x = mconcat [ "<" , t , ">" , x , "</" , t , ">" ]

data Wiki = Wiki ( (Int,Int -> Text) -> (Int,Int -> Text,Text) )

#if MIN_VERSION_base(4,11,0)

instance Semigroup Wiki where
 (Wiki g) <> (Wiki g') =
   Wiki $ \s -> let (i',f',t) = g s
                    (i'',f'',t') = g' (i',f')
                in  (i'',f'',t <> t')

instance Monoid Wiki where
 mempty = Wiki $ \(i,f) -> (i,f,mempty)

#else

instance Monoid Wiki where
 (Wiki g) `mappend` (Wiki g') =
   Wiki $ \s -> let (i',f',t) = g s
                    (i'',f'',t') = g' (i',f')
                in  (i'',f'',t `mappend` t')
 mempty = Wiki $ \(i,f) -> (i,f,mempty)

#endif

text :: Text -> Wiki
text t = Wiki $ \(i,f) -> (i,f,t)

syntaxWiki :: Syntax -> Wiki
syntaxWiki (Raw t) = text t
syntaxWiki (Section n s) =
  let m = 1 + n
      d = text $ replicate m "="
  in d <> syntaxWiki s <> d <> text "\n\n"
syntaxWiki (Bold s) =
  let d = text "'''"
  in  d <> syntaxWiki s <> d
syntaxWiki (Italic s) =
  let d = text "''"
  in  d <> syntaxWiki s <> d
syntaxWiki (Code b t) =
  let f = tag $ if b then "hask" else "haskell"
  in  (text $ f t) <> (if b then mempty else text "\n\n")
syntaxWiki (URL t) = text t
-- Images no supported.
syntaxWiki (IMG _) = mempty
syntaxWiki LaTeX = text "LaTeX"
syntaxWiki HaTeX = text "HaTeX"
syntaxWiki (Math t) = text $ tag "math" t
syntaxWiki (Footnote s) =
  Wiki (\(i,f) ->
   let i0 = i + 1
       Wiki f' = syntaxWiki s
       (_,_,t) = f' (i0,f)
       g = \n -> if n == i0 then t else f n
   in (i+1,g, "[[#Footnotes|" <> tag "sup" (fromString $ show i0) <> "]]")
    )
syntaxWiki (Paragraph s) = syntaxWiki s <> text "\n\n"
syntaxWiki (Append s1 s2) = syntaxWiki s1 <> syntaxWiki s2
syntaxWiki Empty = mempty

initial :: Text
initial = mempty

ending :: Text
ending = mempty

renderWiki :: Wiki -> Text
renderWiki (Wiki f) = initial <> t <> foots <> ending
 where
  (l,footf,t) = f (0 , const mempty)
  foots = unlines $ "\n\n==Footnotes==\n" :
    fmap (\n -> tag "sup" (fromString $ show n) <> ": " <> strip (footf n) <> "\n") [ 1 .. l ]

backend :: IO ()
backend = fmap (strip . renderWiki . syntaxWiki . mconcat . fmap (syntLineBreaks . (Raw "\n\n" <>))) parseSections >>= writeFile (outputName ".wiki")

-- Line breaks

syntLineBreaks :: Syntax -> Syntax
syntLineBreaks (Raw t) = Raw $ nolineBreaks t
syntLineBreaks (Bold s) = Bold $ syntLineBreaks s
syntLineBreaks (Italic s) = Italic $ syntLineBreaks s
syntLineBreaks (Append s1 s2) = Append (syntLineBreaks s1) (syntLineBreaks s2)
syntLineBreaks s = s

nolineBreaks :: Text -> Text
nolineBreaks = intercalate "\n\n" . fmap (unwords . lines) . splitOn "\n\n"
