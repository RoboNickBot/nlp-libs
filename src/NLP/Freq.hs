module NLP.Freq ( cosine
                , freqMap
                , prettyprint
                , Frequency(..)
                , FreqMap(..)
                , FreqList(..) ) where

import qualified Data.Map as M
import qualified Data.List as L

import Data.NGram

type Frequency = Int
type FreqMap g = M.Map g Frequency
type FreqList g = [(g, Frequency)]

prettyprint :: (NGram g) => FreqMap g -> String
prettyprint = concat . fmap showln 
              . fmap (\(g,f) -> (ngshow g,f)) 
              . reverse 
              . L.sortBy (\(_,f) (_,g) -> compare f g) 
              . M.toList
              
showln (sg,f) = sg ++ " " ++ show f ++ "\n"

freqMap :: (NGram g) => [g] -> FreqMap g
freqMap = foldr (\t m -> if M.member t m
                          then M.adjust (+1) t m
                          else M.insert t 1 m)
                M.empty

cosine :: (NGram g) => FreqMap g -> FreqMap g -> Double
cosine a b = dot a b / (len a * len b)

dot :: (NGram g) => FreqMap g -> FreqMap g -> Double
dot a b = (fromIntegral 
           . foldr (\(k,v) p -> v * (l k) + p) 0 
           . M.toList) a
  where l k = case M.lookup k b of
                Just v -> v
                _ -> 0

len :: (NGram g) => FreqMap g -> Double
len = sqrt . fromIntegral . foldr (\a s -> a^2 + s) 0 
      . fmap snd . M.toList
