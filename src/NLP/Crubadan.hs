module NLP.Crubadan (readCrData) where


import qualified Data.Map as M
import qualified Data.Text as T
import Text.ParserCombinators.Parsec

import Data.NGram
import NLP.Freq

crTriGramFile :: FreqMap TriGram -> String
crTriGramFile = prettyprint

readCrData :: String -> IO (Either ParseError (FreqMap TriGram))
readCrData fpath = 
  do s <- readFile fpath 
     let ngs = (fmap M.fromList . parse triGramFile "err") s
     return ngs


triGramFile :: GenParser Char st [(TriGram, Frequency)]
triGramFile = do result <- many line
                 eof
                 return result

line = do a <- letter <|> oneOf "<>"
          b <- letter <|> oneOf "<>"
          c <- letter <|> oneOf "<>"
          char ' '
          freq <- many (noneOf "\n")
          char '\n'
          return (TriGram (toTok a) (toTok b) (toTok c), read freq)

