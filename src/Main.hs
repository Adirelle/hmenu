{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Maybe
import qualified Data.Text          as T
import           HMenu.Path
import           HMenu.Search
import           HMenu.Types
import           HMenu.Xdg
import           System.Environment
import           Text.Printf

main :: IO ()
main = do
    desktopEntries <- listDesktopEntries
    pathEntries <- listPathEntries
    let entries = desktopEntries ++ pathEntries
        index = createIndex entries
    args <- map T.pack <$> getArgs
    let terms = T.intercalate " " args
        results = search terms index
    forM_ results $ \(e, w) ->
        printf "%s - %s - %.03f \n" (title e) (fromMaybe "" (comment e)) (100.0 * w)
