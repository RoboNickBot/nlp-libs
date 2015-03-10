module Language.Identify.Parse (readCrData) where

import Data.Char (isSpace, isAlpha, toLower)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.ParserCombinators.Parsec

import Language.Identify.Types

readCrData :: T.Text -> IO (Either ParseError (FreqList TriGram))
readCrData fpath = 
  do s <- readFile (T.unpack fpath) 
     let ngs = (fmap M.fromList . parse triGramFile "err") s
     return ngs

instance NGram TriGram where 
  ngrams = ngcount . concat . fmap triGrams
           . smooth . T.unpack

ngcount :: (NGram a) => [a] -> FreqList a
ngcount = foldr (\t m -> if M.member t m
                            then M.adjust (+1) t m
                            else M.insert t 1 m)
                M.empty

triGrams :: [NGramTok] -> [TriGram]
triGrams (a:b:c:ts) = (TriGram a b c) : triGrams (b:c:ts)
triGrams _ = []
        
smooth :: String -> [[NGramTok]]
smooth = fmap wordToTok . words . fmap toLower 
         . filter (\c -> isAlpha c || isSpace c)

triGramFile :: GenParser Char st [(TriGram, Freq)]
triGramFile = do result <- many line
                 eof
                 return result

line = do a <- letter
          b <- letter
          c <- letter
          char ' '
          freq <- many (noneOf "\n")
          char '\n'
          return (TriGram (toTok a) (toTok b) (toTok c), read freq)

