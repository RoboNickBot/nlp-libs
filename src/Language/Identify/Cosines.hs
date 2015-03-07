module Language.Identify.Cosines (cosine) where

import qualified Data.Map as M

import Language.Identify.Types

cosine :: NGrams -> NGrams -> Double
cosine a b = dot a b / (len a * len b)

dot :: NGrams -> NGrams -> Double
dot a b = (fromIntegral 
           . foldr (\(k,v) p -> v * (l k) + p) 0 
           . M.toList) a
  where l k = case M.lookup k b of
                Just v -> v
                _ -> 0

len :: NGrams -> Double
len = sqrt . fromIntegral . foldr (\a s -> a^2 + s) 0 
      . fmap snd . M.toList
