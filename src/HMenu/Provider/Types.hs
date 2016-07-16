{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Provider.Types where

import           ClassyPrelude
import           Data.Hashable

import           HMenu.ScanDirs
import           HMenu.Types    (Entry)

type Filter = FilePath -> IO Bool

type Hash = Int
type Hasher = [FilePath] -> IO Hash

type Converter = FilePath -> IO [Entry]

data EntryProvider = EntryProvider {
        toHash    :: Hash,
        toEntries :: IO [Entry]
    }

instance Hashable EntryProvider where
    hash = toHash
    hashWithSalt x p = x `hashWithSalt` p

fileBasedProvider :: [FilePath] -> Filter -> Hasher -> Converter -> IO EntryProvider
fileBasedProvider dirs filter hasher converter = do
    files <- trace "scanning files" $ scanDirs filter dirs
    hash <- trace "hashing files" $ hasher files
    return $ trace "returning provider" $ EntryProvider hash (entries files)
    where
        entries :: [FilePath] -> IO [Entry]
        entries files = trace "converting files" $ concatMap converter files

metaProvider :: [EntryProvider] -> EntryProvider
metaProvider providers = trace "returning merge provider" $ EntryProvider concatHash concatEntries
    where
        concatHash :: Hash
        concatHash = trace "merging hashes" $ foldr (flip hashWithSalt . toHash) 0 providers
        concatEntries :: IO [Entry]
        concatEntries = trace "merging entry lists" $ concat <$> mapM toEntries providers
