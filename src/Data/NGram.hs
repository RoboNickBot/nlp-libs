{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.NGram ( NGToken(..)
                  , fromTok
                  , toTok
                  , wordToTok
                  , NGram(..)
                  , TriGram(..) ) where

import qualified Data.Text as T
import qualified Data.Set as S

import Data.CharSet.Unicode.Block (Block(..), blocks)
import Data.CharSet (member)
import Data.Char (isSpace, isAlpha, toLower)

class PlainText x where
  charSeq :: x -> [Char]

instance PlainText T.Text where
  charSeq = T.unpack

instance PlainText [Char] where
  charSeq = id

class (Show t, Read t, Eq t) => Token t where
  tokenize :: (PlainText x) => x -> [t]
  tokToString :: t -> String

data NGToken = WordStart | Letter Char | WordEnd
               deriving (Show, Read, Eq, Ord)

instance Token NGToken where
  tokenize = concat . smooth . charSeq
  tokToString t = [fromTok t]

blocksUsed :: NGToken -> S.Set String
blocksUsed (Letter c) =
  (S.fromList 
   . fmap blockName 
   . filter (\b -> member c (blockCharSet b))) (blocks)
blocksUsed _ = S.empty 

fromTok :: NGToken -> Char
fromTok WordStart = '<'
fromTok WordEnd = '>'
fromTok (Letter c) = c

toTok :: Char -> NGToken
toTok '<' = WordStart
toTok '>' = WordEnd
toTok c = Letter c

wordToTok :: String -> [NGToken]
wordToTok word = [WordStart] ++ (fmap Letter word) ++ [WordEnd]

class (Show g, Read g, Eq g, Ord g) => NGram g where
  ngrams :: T.Text -> [g]
  ngshow :: g -> String
  ngblocks :: g -> S.Set String

class (Show g, Read g, Eq g, Token t) => Feature g t where
  features  :: [t] -> [g]
  fToString :: g -> String

data TriGram tok = TriGram { tri1 :: tok
                           , tri2 :: tok
                           , tri3 :: tok }
                   deriving (Show, Read, Eq, Ord)

instance NGram (TriGram NGToken) where
  ngrams = concat . fmap triGrams . smooth . T.unpack
  ngshow (TriGram a b c) = [fromTok a, fromTok b, fromTok c]
  ngblocks (TriGram a b c) = S.unions (fmap blocksUsed [a,b,c])

instance Feature (TriGram NGToken) NGToken where
  features (a:b:c:ts) = 
    (TriGram a b c) : (if c == WordEnd
                          then features ts
                          else features (b:c:ts))
  features _ = []
  fToString (TriGram a b c) = concat (fmap tokToString [a,b,c])

triGrams :: [NGToken] -> [TriGram NGToken]
triGrams (a:b:c:ts) = (TriGram a b c) : triGrams (b:c:ts)
triGrams _ = []

smooth :: [Char] -> [[NGToken]]
smooth = fmap wordToTok . words . fmap toLower 
         . filter (\c -> isAlpha c 
                         || isSpace c 
                         || c == '\''
                         || c == '-')
