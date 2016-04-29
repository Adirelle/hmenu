{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Search (
    createIndex,
    search,
    Index,
    tokenCount
) where

import           ClassyPrelude       hiding (Index)
import           Control.DeepSeq
import           Control.Monad.State (State, execState, modify')
import           Data.Binary
import qualified Data.HashMap.Strict as HM
import           Data.Text           (inits)

import           Data.BinaryRef
import           HMenu.Types

type Token       = Text
type Weight      = Double
type WeightMap k = HashMap k Weight

type Index_ = HashMap Token (WeightMap Entry)

newtype Index = Index Index_
                deriving (Eq, Show, Generic)

instance NFData Index

type Indexer = State Index_ ()

createIndex :: [Entry] -> Index
createIndex entries =
    let rawIndex = execState (mapM_ indexEntry entries) mempty
        tokens = mapToList rawIndex
        filtered = mapMaybe weighten tokens
    in Index $ mapFromList filtered
    where
        count = fromIntegral $ length entries
        weighten (t, m) =
            let f = (count - fromIntegral (length m)) / count
            in if f < 0.1
                then Nothing
                else Just (t, map (f *) m)

tokenCount :: Index -> Int
tokenCount (Index i) = length i

search :: Index -> Text -> [Entry]

search (Index index) terms =
    let tokens  = tokenize terms
        matches = mapMaybe (`lookup` index) tokens
        pairs = unionsWith (+) matches
    in map fst $ sortOn (Down . snd) $ mapToList pairs

indexEntry :: Entry -> Indexer
indexEntry e = do
    indexField 1.0 $ title e
    forM_ (comment e) (indexField 0.8)
    indexField 0.6 $ command e
    where
        indexField :: Weight -> Text -> Indexer
        indexField w t = do
            let ts = tokenize t
                d =  fromIntegral $ length ts
            forM_ ts $ indexToken (w / d)
        indexToken :: Weight -> Text -> Indexer
        indexToken t w = modify' $ addToken e t w

addToken :: Entry -> Weight -> Token -> Index_ -> Index_
addToken e w = alterMap (alterEntry w)
    where
        alterEntry :: Weight -> Maybe (WeightMap Entry) -> Maybe (WeightMap Entry)
        alterEntry w Nothing  = Just $ singletonMap e w
        alterEntry w (Just m) = Just $ insertWith (+) e w m

tokenize :: Text -> [Text]
tokenize t = concatMap (nGrams 3 8) (words $ toCaseFold t)

nGrams :: Int -> Int -> Text -> [Text]
nGrams a b t = t : drop a (take (b+1) $ inits t)

instance Binary Index where
    put (Index i) = putWithRefs go
        where
            go :: PutRef Entry
            go = do
                lift (put $ HM.size i)
                oforM_ (HM.toList i) putTokenPair
            putTokenPair (t, es) = do
                lift $ put t
                lift (put $ HM.size es)
                oforM_ (HM.toList es) putEntryPair
            putEntryPair (e, w) = do
                putRef e
                lift $ put w
    get = do
        i <- getWithRefs go
        return $ Index i
        where
            go :: GetRef Entry Index_
            go = do
                il <- lift get
                ts <- replicateM il getTokenPair
                return $ HM.fromList ts
            getTokenPair :: GetRef Entry (Token, WeightMap Entry)
            getTokenPair = do
                t <- lift (get :: Get Token)
                el <- lift get
                es <- replicateM el getEntryPair
                return (t, HM.fromList es)
            getEntryPair :: GetRef Entry (Entry, Weight)
            getEntryPair = do
                e <- getRef
                w <- lift get
                return (e, w)
