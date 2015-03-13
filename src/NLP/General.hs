{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module NLP.General ( NGToken(..)
                   , fromTok
                   , toTok
                   , wordToTok
                   , Token(..)
                   , FeatureOn(..)
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

class ( Show t, Read t, Eq t
      , PrettyPrint t) => Token t where
  tokens :: (PlainText x) => x -> [t]

class ( Show f, Read f, Eq f
      , Token t, PrettyPrint f) => FeatureOn f t where
  features  :: [t] -> [f]

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

{-
        
class (Show g, Read g, Eq g, Ord g) => NGram g where
  ngrams :: T.Text -> [g]
  ngshow :: g -> String
  ngblocks :: g -> S.Set String
  
  -}


data TriGram tok = TriGram { tri1 :: tok
                           , tri2 :: tok
                           , tri3 :: tok }
                   deriving (Show, Read, Eq, Ord)

{-
instance NGram (TriGram NGToken) where
  ngrams = concat . fmap triGrams . smooth . T.unpack
  ngshow (TriGram a b c) = [fromTok a, fromTok b, fromTok c]
  ngblocks (TriGram a b c) = S.unions (fmap blocksUsed [a,b,c])
  -}
  
instance FeatureOn (TriGram NGToken) NGToken where
  features (a:b:c:ts) = 
    (TriGram a b c) : (if c == WordEnd
                          then features ts
                          else features (b:c:ts))
  features _ = []

instance PrettyPrint (TriGram NGToken) where
  prettyprint (TriGram a b c) = concat (fmap prettyprint [a,b,c])

data UBlock = UBlock { ubname :: String } 
              deriving (Show, Read, Eq, Ord)

instance FeatureOn UBlock NGToken where
  features = fmap UBlock . foldr (\s -> (++) (blocksUsed s)) []
  
instance PrettyPrint UBlock where  
  prettyprint ub = [ubname ub]

triGrams :: [NGToken] -> [TriGram NGToken]
triGrams (a:b:c:ts) = (TriGram a b c) : triGrams (b:c:ts)
triGrams _ = []

smooth :: [Char] -> [[NGToken]]
smooth = fmap wordToTok . words . fmap toLower 
         . filter (\c -> isAlpha c 
                         || isSpace c 
                         || c == '\''
                         || c == '-')
