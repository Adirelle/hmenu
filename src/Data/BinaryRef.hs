{-# LANGUAGE NoImplicitPrelude #-}

module Data.BinaryRef (
    PutRef,
    putWithRefs,
    putRef,
    GetRef,
    getWithRefs,
    getRef
) where

import           ClassyPrelude
import qualified Control.Monad.Trans.State as ST
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.HashMap.Strict       as HM
import qualified Data.IntMap.Strict        as IM

data Ref a = Plain a
           | BackRef Int
           deriving (Show, Eq, Ord)

instance Binary a => Binary (Ref a) where
    put (Plain x)   = putWord8 0 >> put x
    put (BackRef i) = putWord8 1 >> put i
    get = do
        t <- getWord8
        case t of
            0 -> fmap Plain get
            1 -> fmap BackRef get

type PutRefState a = (Int, HashMap a (Ref a))
type PutRef a = ST.StateT (PutRefState a) PutM ()

putRef :: (Binary a, Hashable a, Eq a) => a -> PutRef a
putRef x = do
    (next, refs) <- ST.get
    r <- case HM.lookup x refs of
        Just i -> return i
        Nothing -> do
            ST.put (next + 1, insertMap x (BackRef next) refs)
            return $ Plain x
    lift $ put r

putWithRefs :: (Binary a, Hashable a, Eq a) => PutRef a -> Put
putWithRefs f = ST.evalStateT f (1, HM.empty)

type GetRefState a = (Int, IntMap a)
type GetRef a b = ST.StateT (GetRefState a) Get b

getRef :: Binary a => GetRef a a
getRef = do
    r <- lift get
    case r of
        Plain x -> do
            ST.modify' $ \(next, refs) -> (next + 1, IM.insert next x refs)
            return x
        BackRef i -> do
            (_, refs) <- ST.get
            case IM.lookup i refs of
                Nothing -> error $ "Invalid backreference: " ++ show i
                Just x -> return x

getWithRefs :: Binary a => GetRef a b -> Get b
getWithRefs f = ST.evalStateT f (1, IM.empty)
