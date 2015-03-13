module NLP.Freq ( cosine
                , dot
                , len
                , mkFreqList
                , Frequency
                , FreqList(..) ) where

import qualified Data.Map as M
import qualified Data.List as L

import Data.NGram

type Frequency = Int

data FreqList f = 
  FreqList { freqMap :: M.Map f Frequency}

mkFreqList :: (Ord f) => [f] -> FreqList f
mkFreqList fs = 
  FreqList (foldr (\t m -> if M.member t m
                              then M.adjust (+1) t m
                              else M.insert t 1 m)
                  M.empty fs)

instance PrettyPrint f => PrettyPrint (FreqList f) where
  prettyprint = fmap showln 
                . fmap (\(feat,freq) -> 
                          ((concat . prettyprint) feat,freq)) 
                . reverse 
                . L.sortBy (\(_,a) (_,b) -> compare a b) 
                . M.toList
                . freqMap

showln (sg,f) = sg ++ " " ++ show f

{-
freqMap :: (NGram g) => [g] -> FreqMap g
freqMap = foldr (\t m -> if M.member t m
                          then M.adjust (+1) t m
                          else M.insert t 1 m)
                M.empty
        -}

cosine :: Ord f => FreqList f -> FreqList f -> Double
cosine a b = dot a b / (len a * len b)

dot :: Ord f => FreqList f -> FreqList f -> Double
dot a b = (fromIntegral 
           . foldr (\(k,v) p -> v * (l k) + p) 0 
           . M.toList) (freqMap a)
  where l k = case M.lookup k (freqMap b) of
                Just v -> v
                _ -> 0

len :: FreqList f -> Double
len = sqrt . fromIntegral . foldr (\a s -> a^2 + s) 0 
      . fmap snd . M.toList . freqMap
