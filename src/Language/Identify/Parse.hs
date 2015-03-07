module Language.Identify.Parse (readCrData) where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.ParserCombinators.Parsec

import Language.Identify.Types

readCrData :: T.Text -> IO (Either ParseError NGrams)
readCrData fpath = 
  do s <- readFile (T.unpack fpath) 
     let ngs = (fmap M.fromList . parse ngramFile "err") s
     return ngs

plaintext :: GenParser Char st [(T.Text, Int)]
plaintext = do result <- many word
               eof
               return result
               
word = undefined

ngramFile :: GenParser Char st [(T.Text, Int)]
ngramFile = do result <- many line
               eof
               return result

line = do ngram <- many (noneOf " ")
          char ' '
          freq <- many (noneOf "\n")
          char '\n'
          return (T.pack ngram, read freq)

