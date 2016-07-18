{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module HMenu.Search (
    createIndex,
    search,
    Index,
    tokenCount,
    Indexable(..)
) where

import           ClassyPrelude       hiding (Index)
import           Control.DeepSeq
import           Control.Monad.State (State, execState, modify')
import           Data.Binary
import qualified Data.HashMap.Strict as HM
import           Data.Text           (inits)

import           Data.BinaryRef

type Token       = Text
type Weight      = Double
type WeightMap k = HashMap k Weight

type Index_ a = HashMap Token (WeightMap a)

newtype Index a = Index (Index_ a)
                deriving (Eq, Show, Generic)

instance NFData a => NFData (Index a)

class (Hashable a, Eq a) => Indexable a where
    data IndexableField a
    fieldWeight :: IndexableField a -> Weight
    fieldValue :: IndexableField a -> a -> Maybe Text
    fieldList :: [IndexableField a]

type Indexer a = State (Index_ a) ()

createIndex :: Indexable a => [a] -> Index a
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

tokenCount :: Indexable a => Index a -> Int
tokenCount (Index i) = length i

search :: Indexable a => Index a -> Text -> [a]
search _             terms | null terms = []
search (Index index) terms =
    let tokens  = tokenize terms
        matches = mapMaybe (`lookup` index) tokens
        pairs = unionsWith (+) matches
    in map fst $ sortOn (Down . snd) $ mapToList pairs

indexEntry :: Indexable a => a -> Indexer a
indexEntry e =
    forM_ fieldList $ \f ->
        forM_ (fieldValue f e) $ \v ->
            indexField (fieldWeight f) v
    where
        indexField w t = do
            let ts = tokenize t
                d =  fromIntegral $ length ts
            forM_ ts $ indexToken (w / d)
        indexToken t w = modify' $ addToken e t w

addToken :: Indexable a => a -> Weight -> Token -> Index_ a -> Index_ a
addToken e w = alterMap $ \m ->
    Just $ case m of
        Nothing -> singletonMap e w
        Just m' -> insertWith (+) e w m'

tokenize :: Text -> [Text]
tokenize t = concatMap (nGrams 3 8) (words $ toCaseFold t)

nGrams :: Int -> Int -> Text -> [Text]
nGrams a b t = t : drop a (take (b+1) $ inits t)

instance (Indexable a, Binary a) => Binary (Index a) where
    put (Index i) = putWithRefs go
        where
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
            go = do
                il <- lift get
                ts <- replicateM il getTokenPair
                return $ HM.fromList ts
            getTokenPair = do
                t <- lift (get :: Get Token)
                el <- lift get
                es <- replicateM el getEntryPair
                return (t, HM.fromList es)
            getEntryPair = do
                e <- getRef
                w <- lift get
                return (e, w)
