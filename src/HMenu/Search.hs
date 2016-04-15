{-# LANGUAGE OverloadedStrings #-}

module HMenu.Search (
    createIndex,
    search,
    Index,
    Weight
) where

import           HMenu.Types
import           Data.List
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Ord          (Down (..))
import           Data.Text         (Text)
import qualified Data.Text         as T

type Token = Text
type Weight = Double
type WeightMap k = M.Map k Weight
type Index = M.Map Token (WeightMap Entry)

createIndex :: [Entry] -> Index
createIndex entries =
    let singletons    = [ M.singleton t (M.singleton e w) | e <- entries, (t, w) <- M.toList $ entryTokens e ]
        rawIndex      = M.unionsWith (M.unionWith (+)) singletons
        numEntries    = length entries
        filteredIndex = M.filter ((< numEntries) . M.size) rawIndex
        fixWeights m  = let f = fromIntegral (numEntries - M.size m) / fromIntegral numEntries
                        in alterWeights f m
    in M.map fixWeights filteredIndex

search :: Text -> Index -> [(Entry, Weight)]
search terms index =
    let tokens            = tokenizeQuery 1.0 terms
        allMatches        = mapMaybe lookupToken tokens
        lookupToken (w,t) = do
            entries <- M.lookup t index
            return $ alterWeights w entries
        results           = M.unionsWith (+) allMatches
    in sortOn (Down . snd) $ M.toList results

alterWeights :: Double -> WeightMap k -> WeightMap k
alterWeights f = M.map (f *)

entryTokens :: Entry -> WeightMap Token
entryTokens e =
    let tokens = tokenizeField 1.0 (title e)
                 ++ maybe [] (tokenizeField 0.8) (comment e)
                 ++ tokenizeField 0.6 (command e)
        count = fromIntegral $ length tokens
    in M.unionsWith (+) [ M.singleton t (w / count) | (w, t) <- tokens ]

tokenizeField :: Weight -> Text -> [(Weight, Token)]
tokenizeField w t = tokens w t >>= foldCase >>= edgeNGrams 3 8

tokenizeQuery :: Weight -> Text -> [(Weight, Token)]
tokenizeQuery w t = tokens w t >>= foldCase

tokens :: Weight -> Text -> [(Weight, Token)]
tokens w = map (\t -> (w, t)) . T.words

foldCase :: (Weight, Token) -> [(Weight, Token)]
foldCase token@(w, t) = [token, (w * 0.9, T.toCaseFold t)]

edgeNGrams :: Int -> Int -> (Weight, Token) -> [(Weight, Token)]
edgeNGrams a b token@(w, t) = token : ngrams
    where l = T.length t
          ngrams | l <= a    = []
                 | l <= b    = [ take n | n <- [a..l] ]
                 | otherwise = [ take n | n <- [a..b] ]
          take n = (w * fromIntegral n / fromIntegral l, T.take n t)
