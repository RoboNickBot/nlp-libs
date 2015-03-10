module Language.Identify.Cosines (cosine) where

import qualified Data.Map as M

import Language.Identify.Types

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
