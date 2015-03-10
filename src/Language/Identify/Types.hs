module Language.Identify.Types ( NGramTok(..)
                               , fromTok
                               , toTok
                               , wordToTok
                               , NGram(..)
                               , TriGram(..)
                               , Freq(..)
                               , FreqList(..) ) where

import qualified Data.Map as M
import qualified Data.Text as T

data NGramTok = Start | Letter Char | End
                deriving (Show, Read, Eq, Ord)

fromTok :: NGramTok -> Char
fromTok Start = '<'
fromTok End = '>'
fromTok (Letter c) = c

toTok :: Char -> NGramTok
toTok '<' = Start
toTok '>' = End
toTok c = Letter c

wordToTok :: String -> [NGramTok]
wordToTok word = [Start] ++ (fmap Letter word) ++ [End]

class (Show g, Read g, Eq g, Ord g) => NGram g where
  ngrams :: T.Text -> FreqList g

data TriGram = TriGram NGramTok NGramTok NGramTok
               deriving (Show, Read, Eq, Ord)

type Freq = Int

type FreqList g = M.Map g Freq
