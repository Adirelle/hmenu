{-# LANGUAGE OverloadedStrings #-}

module HMenu.Search (
    createIndex,
    search,
    Index,
    Weight,
    nGrams
) where

import           Control.Monad.State
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Ord            (Down (..))
import qualified Data.Text           as T
import           HMenu.Types

type Token = T.Text
type Weight = Double
type WeightMap k = M.Map k Weight
type Index = M.Map Token (WeightMap Entry)

type Indexer = State Index ()

search :: T.Text -> Index -> [(Entry, Weight)]
search terms index =
    let tokens            = tokenize terms
        allMatches        = mapMaybe (`M.lookup` index) tokens
        results           = M.unionsWith (+) allMatches
    in sortOn (Down . snd) $ M.toList results

createIndex :: [Entry] -> Index
createIndex entries =
    let index = execState (mapM_ indexEntry entries) M.empty
    in M.map weightenTokens index
    where
        weightenTokens :: WeightMap Entry -> WeightMap Entry
        weightenTokens m =
            M.map (factor *) m
            where factor = 1.0 / fromIntegral (length m)

indexEntry :: Entry -> Indexer
indexEntry e = do
    indexField 1.0 $ title e
    forM_ (comment e) (indexField 0.8)
    indexField 0.6 $ command e
    where
        indexField :: Weight -> T.Text -> Indexer
        indexField w t = do
            let ts = tokenize t
                d =  fromIntegral $ length ts
            forM_ ts $ indexToken (w / d)
        indexToken :: Weight -> T.Text -> Indexer
        indexToken t w = modify' $ addToken e t w

addToken :: Entry -> Weight -> Token -> Index -> Index
addToken e w = M.alter (alterEntry w)
    where
        alterEntry :: Weight -> Maybe (WeightMap Entry) -> Maybe (WeightMap Entry)
        alterEntry w Nothing  = Just $ M.singleton e w
        alterEntry w (Just m) = Just $ M.insertWith' (+) e w m

tokenize :: T.Text -> [T.Text]
tokenize t = concatMap (nGrams 3 8) (T.words $ T.toCaseFold t)

nGrams :: Int -> Int -> T.Text -> [T.Text]
nGrams a b t = t : drop a (take (b+1) $ T.inits t)
