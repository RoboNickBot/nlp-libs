module Language.Identify.Parse (readCrData) where

import Data.Char (isSpace, isAlpha, toLower)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.ParserCombinators.Parsec

import Language.Identify.Types

readCrData :: T.Text -> IO (Either ParseError NGrams)
readCrData fpath = 
  do s <- readFile (T.unpack fpath) 
     let ngs = (fmap M.fromList . parse ngramFile "err") s
     return ngs

readPlainText :: T.Text -> NGrams
readPlainText = ngcount . concat . fmap parseWord 
                . smooth . T.unpack

ngcount :: [T.Text] -> NGrams
ngcount = foldr (\t m -> if M.member t m
                            then M.adjust (+1) t m
                            else M.insert t 1 m)
                M.empty

data NGramTok = Start | End | Letter Char

parseWord :: [NGramTok] -> [T.Text]
parseWord (a:b:c:ts) = 
  (T.pack . fmap fromTok) [a,b,c] : parseWord (b:c:ts)
parseWord _ = []
        
fromTok Start = '<'
fromTok End = '>'
fromTok (Letter c) = c

toTok :: String -> [NGramTok]
toTok word = [Start] ++ (fmap Letter word) ++ [End]

smooth :: String -> [[NGramTok]]
smooth = fmap toTok . words . fmap toLower 
         . filter (\c -> isAlpha c || isSpace c)

ngramFile :: GenParser Char st [(T.Text, Int)]
ngramFile = do result <- many line
               eof
               return result

line = do ngram <- many (noneOf " ")
          char ' '
          freq <- many (noneOf "\n")
          char '\n'
          return (T.pack ngram, read freq)

