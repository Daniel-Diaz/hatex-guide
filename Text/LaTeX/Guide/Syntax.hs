
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Text.LaTeX.Guide.Syntax (
   Syntax (..)
 , Text
 , printSyntax
 , parseSyntax
 , parseFile
  ) where

import Text.LaTeX.Base hiding (between)
import Text.Parsec hiding (Empty)
import Text.Parsec.Text ()
import Data.Text
import Data.Text.IO hiding (putStr)
import Data.Int
import Data.Bool
import Prelude (Eq(..),Show(..),FilePath,Enum)

#if MIN_VERSION_base(4,11,0)
import Prelude (Semigroup)
#endif

import Data.Function
import Control.Monad
import qualified Data.List as L
import Data.Char
import Data.Either
import System.IO(IO,hFlush,stdout,putStr)

{- Syntax table

-- Sections
 # ... #  Section
## ... ## Subsection
and so on...

-- Styles
* ... * Bold
/ ... / Italic

-- Code
\{ ... \} Inline
\[ ... \] No-inline

-- Math
$ ... $ Math

-- Utils
< ... >   URL
| ... |   Image
\latex    LaTeX logo
\hatex    HaTeX logo
\f ... \f Footnote

To escape reserved characters, use the backslash (\).

-}

data Syntax =
   -- Plain text
   Raw Text
   -- Features
 | Section Int Syntax
 | Bold Syntax
 | Italic Syntax
 | Code Bool Text -- If True then inline.
 | URL Text
 | IMG Text
 | LaTeX
 | HaTeX
 | Math Text
 | Footnote Syntax
 | Paragraph Syntax
   -- Monoid constructors
 | Append Syntax Syntax
 | Empty
   deriving Show

#if MIN_VERSION_base(4,11,0)

instance Semigroup Syntax where
 Empty <> x = x
 x <> Empty = x
 x <> y = Append x y

instance Monoid Syntax where
 mempty = Empty

#else

instance Monoid Syntax where
 Empty `mappend` x = x
 x `mappend` Empty = x
 x `mappend` y = Append x y
 mempty = Empty

#endif

-- Printer

printSyntax :: Syntax -> Text
printSyntax (Raw t) = concatMap (\c -> if c `L.elem` resChars then "\\" <> singleton c else singleton c) t
printSyntax (Section n s) = let d = replicate n "#" in d <> printSyntax s <> d
printSyntax (Bold s) = "*" <> printSyntax s <> "*"
printSyntax (Italic s) = "/" <> printSyntax s <> "/"
printSyntax (Code b t) = let (d1,d2) = if b then ("\\{","\\}") else ("\\[","\\]")
                         in  d1 <> t <> d2
printSyntax (URL t) = "<" <> t <> ">"
printSyntax (IMG t) = "|" <> t <> "|"
printSyntax LaTeX = "\\LaTeX"
printSyntax HaTeX = "\\HaTeX"
printSyntax (Math t) = let d = "$" in d <> t <> d
printSyntax (Footnote s) = let d = "\\f" in d <> printSyntax s <> d
printSyntax (Paragraph s) = printSyntax s <> "\n\n"
printSyntax (Append s1 s2) = printSyntax s1 <> printSyntax s2
printSyntax Empty = mempty

-- Parser

data ParseItem =
   PSection
 | PBold
 | PItalic
 | PFootnote
   deriving (Eq,Enum)

allParseItems :: [ParseItem]
allParseItems = [ PSection .. ]

parseItem :: ParseItem -> Parser Syntax
---------------------------------------
parseItem PSection = do
 xs <- many1 (char '#')
 let n = L.length xs
 s <- p_SyntaxWith PSection
 _ <- string $ L.replicate n '#'
 return $ Section n s
---------------------------------------
parseItem PBold = p_Chars Bold PBold '*' '*'
---------------------------------------
parseItem PItalic = p_Chars Italic PItalic '/' '/'
---------------------------------------
parseItem PFootnote = between (char 'f') (string "\\f") $ fmap Footnote $ p_SyntaxWith PFootnote

type Parser = Parsec Text (ParseItem -> Bool)

itemTo :: ParseItem -> Bool -> Parser ()
itemTo pi b = modifyState $ \f -> \x -> if x == pi then b else f x

p_SyntaxWith :: ParseItem -> Parser Syntax
p_SyntaxWith pi = between (pi `itemTo` False) (pi `itemTo` True) $ fmap mconcat $ many1 p_Unit

p_Chars :: (Syntax -> a) -> ParseItem -> Char -> Char -> Parser a
p_Chars f pi c1 c2 = fmap f $ between (char c1) (char c2) $ p_SyntaxWith pi

p_Backslash :: Parser Syntax
p_Backslash = do
 char '\\'
 let ps = [ p_InlineCode , p_LaTeX , p_HaTeX , fmap (Raw . fromString . (\c -> ['\\',c])) $ noneOf "f" ]
 f <- getState
 choice $ if f PFootnote then parseItem PFootnote : ps else ps

p_InlineCode :: Parser Syntax
p_InlineCode = do
 char '{'
 xs <- manyTill anyChar $ try $ string "\\}"
 return $ Code True $ fromString xs

p_BlockCode :: Parser Syntax
p_BlockCode = do
 string "\\["
 xs <- manyTill anyChar $ try $ string "\\]"
 return $ Code False $ fromString xs

p_LaTeX :: Parser Syntax
p_LaTeX = string "latex" >> return LaTeX

p_HaTeX :: Parser Syntax
p_HaTeX = string "hatex" >> return HaTeX

p_URL :: Parser Syntax
p_URL = do
 char '<'
 xs <- many $ noneOf ">"
 char '>'
 return $ URL $ fromString xs

p_IMG :: Parser Syntax
p_IMG = do
 char '|'
 xs <- many $ noneOf "|"
 char '|'
 return $ IMG $ fromString xs

p_Math :: Parser Syntax
p_Math = do
 char '$'
 xs <- many $ noneOf "$"
 char '$'
 return $ Math $ fromString xs

p_Raw :: Parser Syntax
p_Raw = fmap (Raw . fromString) $ many1 $ noneOf $ '\n' : resChars

p_Paragraph :: Parser Syntax
p_Paragraph = do
  x  <- p_Unit
  xs <- manyTill p_Unit (try (void $ string "\n\n") <|> eof)
  return $ Paragraph $ mconcat $ x : xs

p_LineBreak :: Parser Syntax
p_LineBreak = do
  _ <- char '\n'
  return $ Raw "\n"

resChars :: [Char]
resChars = "$/\\#<>|*"

p_Unit :: Parser Syntax
p_Unit = do
 f <- getState
 let xs = L.filter f allParseItems
     ts = [ p_LineBreak, p_URL , p_Math , try p_Backslash , p_Raw ]
 choice $ ts <> fmap parseItem xs

p_TopLevel :: Parser Syntax
p_TopLevel = choice
  [ p_LineBreak
  , parseItem PSection
  , p_IMG
  , try p_BlockCode
  , p_Paragraph
    ]

p_Syntax :: Parser Syntax
p_Syntax = fmap mconcat $ many $ p_TopLevel

parseSyntax :: FilePath -> Text -> Either ParseError Syntax
parseSyntax = runParser (withEOF p_Syntax) (const True)

withEOF :: (Stream s m t, Show t) => ParsecT s u m b -> ParsecT s u m b
withEOF = (>>= (eof >>) . return)

-- IO

putStr' :: String -> IO ()
putStr' = (>> hFlush stdout) . putStr

parseFile :: FilePath -> IO Syntax
parseFile fp = do
 putStr' $ mconcat [ "Reading file " , fp , "... " ]
 putStrLn "Done."
 t <- readFile fp
 putStr' $ mconcat [ "Parsing " , fp , "... " ]
 case parseSyntax fp t of
  Left e -> putStrLn "ParseFailed." >> fail (show e)
  Right s -> putStrLn "ParseOk." >> return s
