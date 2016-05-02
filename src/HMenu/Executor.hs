{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Executor where

import           ClassyPrelude        hiding (on)
import           System.Posix.Process

import           HMenu.Types

launch :: Entry -> IO ()
launch Entry {command = cmd} = do
    let (binary:args) = words $ unpack cmd
    executeFile binary False args Nothing
