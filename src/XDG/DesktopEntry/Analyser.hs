{-# LANGUAGE NoImplicitPrelude #-}

module XDG.DesktopEntry.Analyser (
    analyse
) where

import           ClassyPrelude              hiding (optional)
import           Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict        as HM

import qualified Data.Locale                as L
import           XDG.DesktopEntry.Parser
import           XDG.DesktopEntry.Types

type ErrorOr = Either String
type GroupReader = ReaderT Values ErrorOr

analyse :: Text -> ErrorOr DesktopEntry
analyse t = parse t >>= analyseMainGroup

analyseMainGroup :: Groups -> ErrorOr DesktopEntry
analyseMainGroup g = withGroup g "Desktop Entry" $ do
    t <- required "Type" :: GroupReader Text
    DesktopEntry <$> required "Name"
                 <*> optional "GenericName"
                 <*> boolean "NoDisplay"
                 <*> optional "Comment"
                 <*> optional "Icon"
                 <*> boolean "Hidden"
                 <*> subType t
    where
        subType :: Text -> GroupReader EntryKind
        subType "Application" = do
            d <- boolean "DBusActivatable"
            a <- optional "Actions" :: GroupReader (Maybe [Text])
            e <- optional "Exec"
            e' <- lift $ case (d, e) of
                (False, Nothing) -> Left "Exec is required"
                (_,     x)       -> Right x
            a <- optional "Actions"
            let a' = fmap (rights . map analyseAction) a
            Application <$> return d
                        <*> optional "TryExec"
                        <*> return e'
                        <*> optional "Path"
                        <*> boolean "Terminal"
                        <*> return a'
                        <*> optional "Keywords"
        subType "Link"   = Link <$> required "URL"
        subType "Folder" = return Folder
        subType t        = fail $ "Invalid entry type: " ++ unpack t
        analyseAction :: Text -> ErrorOr Action
        analyseAction n = withGroup g ("Desktop Action " ++ n) $
            Action <$> required "Name"
                   <*> optional "Icon"
                   <*> optional "Exec"

withGroup :: Groups -> Text -> GroupReader a -> ErrorOr a
withGroup e n r =
    case HM.lookup n e of
        Nothing -> Left $ "Missing group: " ++ unpack n
        Just vs -> runReaderT r vs

optional :: FromValue a => Text -> GroupReader (Maybe a)
optional k = do
    v <- asks $ HM.lookup k
    lift $ case v of
        Nothing -> Right Nothing
        Just x  -> Just <$> unwrapValue x

required :: FromValue a => Text -> GroupReader a
required k = do
    v <- asks $ HM.lookup k
    lift $ case v of
        Nothing -> Left $ "Required value not found: " ++ unpack k
        Just x  -> unwrapValue x

boolean :: Text -> GroupReader Bool
boolean t = fromMaybe False <$> optional t
