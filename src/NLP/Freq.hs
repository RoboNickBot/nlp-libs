module NLP.Freq ( cosine
                , freqList
                , Frequency(..)
                , FreqList(..) ) where

import qualified Data.Map as M

import Data.NGram

type Frequency = Int
type FreqList g = M.Map g Frequency

freqList :: (NGram g) => [g] -> FreqList g
freqList = foldr (\t m -> if M.member t m
                           then M.adjust (+1) t m
                           else M.insert t 1 m)
                 M.empty


cosine :: (NGram g) => FreqList g -> FreqList g -> Double
cosine a b = dot a b / (len a * len b)

dot :: (NGram g) => FreqList g -> FreqList g -> Double
dot a b = (fromIntegral 
           . foldr (\(k,v) p -> v * (l k) + p) 0 
           . M.toList) a
  where l k = case M.lookup k b of
                Just v -> v
                _ -> 0

len :: (NGram g) => FreqList g -> Double
len = sqrt . fromIntegral . foldr (\a s -> a^2 + s) 0 
      . fmap snd . M.toList
