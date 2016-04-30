{-# LANGUAGE NoImplicitPrelude #-}

module Xdg.DesktopEntry.Analyser
     where

import           ClassyPrelude              hiding (optional)
import           Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict        as HM

import qualified Data.Locale                as L
import           Xdg.DesktopEntry.Parser
import           Xdg.DesktopEntry.Types

type Error = String
type Faillible = Either Error
type GroupReader = ReaderT Values Faillible

analyse :: Text -> Faillible DesktopEntry
analyse t = parse t >>= analyseMainGroup

analyseMainGroup :: Groups -> Faillible DesktopEntry
analyseMainGroup g =
    withGroup g "Desktop Entry" $ do
        t  <- required string "Type"
        cm <- analyseCommon
        case t of
            "Application" -> analyseApplication cm
            "Link"        -> analyseLink cm
            "Folder"      -> analyseFolder cm
            _             -> lift $ Left $ "Invalid entry type: " ++ unpack t

analyseCommon :: GroupReader CommonDesc
analyseCommon = do
    n  <- required localestring "Name"
    gn <- optional localestring "GenericName"
    nd <- boolean "NoDisplay"
    c  <- optional localestring "Comment"
    i  <- optional localestring "Icon"
    h  <- boolean "Hidden"
    return $ CommonDesc n gn nd c i h

analyseApplication :: CommonDesc -> GroupReader DesktopEntry
analyseApplication cm = do
    d  <- boolean "DBusActivatable"
    te <- optional string "TryExec"
    e  <- if d
          then optional string "Exec"
          else Just <$> required string "Exec"
    p  <- optional string "Path"
    t  <- boolean "Terminal"
    let a = []
        k = error "not yet implemented"
    return $ Application cm d te e p t a k

analyseLink :: CommonDesc -> GroupReader DesktopEntry
analyseLink cm = do
    u <- required string "URL"
    return $ Link cm u

analyseFolder :: CommonDesc -> GroupReader DesktopEntry
analyseFolder cm = return $ Folder cm

withGroup :: Groups -> Text -> GroupReader a -> Faillible a
withGroup e n r =
    maybe notFound analyse group
    where
        notFound = Left $ "Group not found: " ++ unpack n
        analyse  = runReaderT r
        group    = HM.lookup n e

mapMaybeValue :: Faillible a -> (Value -> Faillible a) -> Text -> GroupReader a
mapMaybeValue e f n = do
    v <- asks $ HM.lookup n
    lift $ maybe e f v

required :: (Value -> Faillible a) -> Text -> GroupReader a
required f n = mapMaybeValue (Left $ "Required value not found: " ++ unpack n) f n

optional :: (Value -> Faillible a) -> Text -> GroupReader (Maybe a)
optional f = mapMaybeValue (Right Nothing) (\x -> Just <$> f x)

boolean :: Text -> GroupReader Bool
boolean n =
    mapMaybeValue (Right False) cast n
    where
        cast (BoolValue b) = Right b
        cast _             = Left $ "Boolean expected for " ++ unpack n

string :: Value -> Faillible Text
string (StringValue t)       = Right t
string (LocaleStringValue m) = maybe (Left "") Right $ HM.lookup L.Default m
string _                     = Left "String expected"

localestring :: Value -> Faillible (Localized Text)
localestring (StringValue t)       = Right $ Localized (HM.singleton L.Default t)
localestring (LocaleStringValue m) = Right $ Localized m
localestring _                     = Left "String expected"
