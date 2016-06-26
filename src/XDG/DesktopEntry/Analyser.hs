{-# LANGUAGE NoImplicitPrelude #-}

module XDG.DesktopEntry.Analyser (
    readDesktopEntry
) where

import           ClassyPrelude              hiding (optional)
import           Control.Monad.Trans.Reader
import           Data.Attoparsec.Text       as A
import qualified Data.HashMap.Strict        as HM
import           Data.Text                  (strip)
import qualified Data.Text.IO               as DTI

import qualified Data.Locale                as L
import           XDG.DesktopEntry.Parser    as P
import           XDG.DesktopEntry.Types

type ErrorOr = Either String
type GroupReader = ReaderT Values ErrorOr

readDesktopEntry :: L.Locale -> FilePath -> IO (ErrorOr DesktopEntry)
readDesktopEntry l p = do
    c <- DTI.readFile p
    return $ expandExecVars l p <$> analyse c

analyse :: Text -> ErrorOr DesktopEntry
analyse t = P.parse t >>= analyseMainGroup

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

class ActionType a where
    name :: L.Locale -> a -> Maybe Text
    icon :: L.Locale -> a -> Maybe Text
    exec :: a -> Maybe Text
    withExec :: a -> Maybe Text -> a

instance ActionType DesktopEntry where
    name l de = getLocalized l $ deName de
    icon l de = getLocalized l =<< deIcon de
    exec DesktopEntry { sub = Application { appExec = e } } = e
    exec _ = Nothing
    withExec de@DesktopEntry { sub = app@Application {} } e = de { sub = app { appExec = e } }
    withExec de _ = de

instance ActionType Action where
    name l a = getLocalized l $ aName a
    icon l a = getLocalized l =<< aIcon a
    exec = aExec
    withExec a e = a { aExec = e }

expandExecVars :: L.Locale -> FilePath -> DesktopEntry -> DesktopEntry
expandExecVars l p de@DesktopEntry { sub = app@Application { appActions = as } } =
    let de' = expandActionExec de
        as' = fmap (map expandActionExec) as
        app' = app { appActions = as' }
    in de' { sub = app' }
    where
        expandActionExec :: ActionType a => a -> a
        expandActionExec a =
            let e = exec a
                e' = fmap expand e
            in withExec a e'
            where
                expand  = unwords . mapMaybe expandPart . splitArgs
                expandPart "%%" = Just "%"
                expandPart "%i" = (asText "--icon " ++) <$> icon l a
                expandPart "%c" = name l a
                expandPart "%k" = Just $ pack p
                expandPart "%f" = Nothing
                expandPart "%F" = Nothing
                expandPart "%u" = Nothing
                expandPart "%U" = Nothing
                expandPart "%d" = Nothing
                expandPart "%D" = Nothing
                expandPart "%n" = Nothing
                expandPart "%N" = Nothing
                expandPart "%v" = Nothing
                expandPart "%m" = Nothing
                expandPart t = Just t
expandExecVars _ _ de = trace "expandExecVars: no changes" de

data APState = Plain | Quoted Char | Escape APState deriving (Show)

splitArgs :: Text -> [Text]
splitArgs = either error id . parseOnly args . strip
    where
        args = sepBy' arg (space >> skipSpace)
        arg = scan Plain step
        step (Escape p) _                         = Just p
        step s          '\\'                      = Just $ Escape s
        step (Quoted d) c    | d == c             = Just Plain
        step Plain      '\''                      = Just $ Quoted '\''
        step Plain      '"'                       = Just $ Quoted '"'
        step Plain      c   | isHorizontalSpace c = Nothing
        step s          _                         = Just s
