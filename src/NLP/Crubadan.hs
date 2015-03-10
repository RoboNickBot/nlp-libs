module NLP.Crubadan (readCrData) where


import qualified Data.Map as M
import qualified Data.Text as T
import Text.ParserCombinators.Parsec

import Data.NGram
import NLP.Freq

readCrData :: T.Text -> IO (Either ParseError (FreqList TriGram))
readCrData fpath = 
  do s <- readFile (T.unpack fpath) 
     let ngs = (fmap M.fromList . parse triGramFile "err") s
     return ngs


triGramFile :: GenParser Char st [(TriGram, Frequency)]
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

