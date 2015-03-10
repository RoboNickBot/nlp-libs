module Data.NGram ( NGToken(..)
                  , fromTok
                  , toTok
                  , wordToTok
                  , NGram(..)
                  , TriGram(..) ) where

import qualified Data.Text as T
import Data.Char (isSpace, isAlpha, toLower)


data NGToken = Start | Letter Char | End
               deriving (Show, Read, Eq, Ord)

fromTok :: NGToken -> Char
fromTok Start = '<'
fromTok End = '>'
fromTok (Letter c) = c

toTok :: Char -> NGToken
toTok '<' = Start
toTok '>' = End
toTok c = Letter c

wordToTok :: String -> [NGToken]
wordToTok word = [Start] ++ (fmap Letter word) ++ [End]

class (Show g, Read g, Eq g, Ord g) => NGram g where
  ngrams :: T.Text -> [g]
  ngshow :: g -> String

data TriGram = TriGram NGToken NGToken NGToken
               deriving (Show, Read, Eq, Ord)

instance NGram TriGram where 
  ngrams = concat . fmap triGrams . smooth . T.unpack
  ngshow (TriGram a b c) = [fromTok a, fromTok b, fromTok c]



triGrams :: [NGToken] -> [TriGram]
triGrams (a:b:c:ts) = (TriGram a b c) : triGrams (b:c:ts)
triGrams _ = []

smooth :: String -> [[NGToken]]
smooth = fmap wordToTok . words . fmap toLower 
         . filter (\c -> isAlpha c || isSpace c)
