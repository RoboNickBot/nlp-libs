{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module NLP.General ( NGToken(..)
                   , fromTok
                   , toTok
                   , wordToTok
                   , Token(..)
                   , Feature(..)
                   , FeatureOf(..)
                   , MetaFeatureOf(..)
                   , PrettyPrint(..)
                   , TriGram(..)
                   , UBlock(..) ) where

import qualified Data.Text as T
import qualified Data.Set as S

import Data.CharSet.Unicode.Block (Block(..), blocks)
import Data.CharSet (member)
import Data.Char (isSpace, isAlpha, toLower)

class PrettyPrint p where
  prettyprint :: p -> [String]

class PlainText x where
  charSeq :: x -> [Char]

class ( Show t, Read t, Eq t, Ord t
      , PrettyPrint t ) => Token t where
  tokens :: (PlainText x) => x -> [t]

class ( Show f, Read f, Eq f, Ord f
      , PrettyPrint f ) => Feature f

class (Feature f, Token t) => FeatureOf f t where
  features  :: [t] -> f
  
class (Feature f, Feature g) => MetaFeatureOf f g where
  metaFeatures :: g -> f

instance PlainText T.Text where
  charSeq = T.unpack

instance PlainText [Char] where
  charSeq = id
  
data NGToken = WordStart | Letter Char | WordEnd
               deriving (Show, Read, Eq, Ord)

instance PrettyPrint NGToken where
  prettyprint t = [[fromTok t]]

instance Token NGToken where
  tokens = concat . smooth . charSeq

blocksUsed :: NGToken -> [String]
blocksUsed (Letter c) =
  (fmap blockName 
   . filter (\b -> member c (blockCharSet b))) (blocks)
blocksUsed _ = []

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

data TriGram tok = TriGram { tri1 :: tok
                           , tri2 :: tok
                           , tri3 :: tok }
                   deriving (Show, Read, Eq, Ord)

instance Token t => PrettyPrint (TriGram t) where
  prettyprint (TriGram a b c) = concat (fmap prettyprint [a,b,c])

instance Token t => PrettyPrint [TriGram t] where
  prettyprint ((TriGram a b c):ts) = 
    concat (fmap prettyprint [a,b,c]) ++ prettyprint ts

instance Token t => Feature (TriGram t)
instance Token t => Feature [TriGram t]

instance FeatureOf [TriGram NGToken] NGToken where
  features (a:b:c:ts) = 
    (TriGram a b c) : (if c == WordEnd
                          then features ts
                          else features (b:c:ts))
  features _ = []


data UBlock = UBlock { ubname :: String } 
              deriving (Show, Read, Eq, Ord)
  
instance PrettyPrint UBlock where  
  prettyprint ub = [ubname ub]

instance PrettyPrint [UBlock] where
  prettyprint us = concat (fmap prettyprint us)

instance Feature UBlock
instance Feature [UBlock]

instance FeatureOf [UBlock] NGToken where
  features = fmap UBlock . foldr (\s -> (++) (blocksUsed s)) []

triGrams :: [NGToken] -> [TriGram NGToken]
triGrams (a:b:c:ts) = (TriGram a b c) : triGrams (b:c:ts)
triGrams _ = []

smooth :: [Char] -> [[NGToken]]
smooth = fmap wordToTok . words . fmap toLower 
         . filter (\c -> isAlpha c 
                         || isSpace c 
                         || c == '\''
                         || c == '-')
