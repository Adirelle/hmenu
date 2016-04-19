{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HMenu.Search (
    createIndex,
    search,
    Index
) where

import           Control.DeepSeq
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Ord            (Down (..))
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           HMenu.Types

type Map = M.Map
type Token = T.Text
type Weight = Double
type WeightMap k = Map k Weight

newtype Index = Index (Map Token (WeightMap Entry))
                deriving (Eq, Show, Generic)

instance NFData Index

type Indexer = State Index ()

createIndex :: [Entry] -> Index
createIndex entries =
    let (Index rawIndex) = execState (mapM_ indexEntry entries) (Index M.empty)
    in Index $ M.mapMaybe applyWeight rawIndex
    where
        count = fromIntegral $ length entries
        applyWeight :: WeightMap Entry -> Maybe (WeightMap Entry)
        applyWeight m =
            let x = (count - fromIntegral (length m)) / count
            in if x < 0.1 then Nothing else Just $ M.map (x *) m

search :: Index -> T.Text -> [Entry]
search (Index index) terms =
    let tokens  = tokenize terms
        matches = mapMaybe (`M.lookup` index) tokens
        pairs = M.unionsWith (+) matches
    in map fst $ sortOn (Down . snd) $ M.toList pairs

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
addToken e w t (Index i) = Index $ M.alter (alterEntry w) t i
    where
        alterEntry :: Weight -> Maybe (WeightMap Entry) -> Maybe (WeightMap Entry)
        alterEntry w Nothing  = Just $ M.singleton e w
        alterEntry w (Just m) = Just $ M.insertWith (+) e w m

tokenize :: T.Text -> [T.Text]
tokenize t = concatMap (nGrams 3 8) (T.words $ T.toCaseFold t)

nGrams :: Int -> Int -> T.Text -> [T.Text]
nGrams a b t = t : drop a (take (b+1) $ T.inits t)
