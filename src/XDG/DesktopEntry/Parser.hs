{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module XDG.DesktopEntry.Parser (
    Groups,
    Values,
    Value(..),
    FromValue,
    unwrapValue,
    XDG.DesktopEntry.Parser.parse
) where

import           ClassyPrelude          hiding (group, insert, union)
import           Data.Attoparsec.Text   as A
import           Data.Char              (isAscii, isControl)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T

import           Data.Locale
import           XDG.DesktopEntry.Types

type Groups = HashMap Text Values
type Values = HashMap Text Value

type ErrorOr = Either String

data Value = Textual LocalizableValue
           | Boolean Bool
           deriving (Eq, Show)

data LocalizableValue = WithLocale (Localized TextualValue)
                      | ASCII TextualValue
                      deriving (Eq, Show)

data TextualValue = Single Text
                  | Many   [Text]
                  deriving (Eq, Show)

instance Semigroup Value where
    Textual a <> Textual b = Textual $ a <> b
    _         <> b         = b

instance Semigroup LocalizableValue where
    WithLocale a     <> WithLocale b             = WithLocale $ a `union` b
    ASCII      a     <> WithLocale (Localized b) = WithLocale $ Localized $ insertWith (<>) Default a b
    a@(WithLocale _) <> b@(ASCII _)              = b <> a
    _                <> b                        = b


instance Semigroup TextualValue where
    Many as  <> Many bs  = Many $ as ++ bs
    Many as  <> Single b = Many $ as `snoc` b
    Single a <> Many bs  = Many $ a : bs
    _        <> b        = b

parse :: Text -> ErrorOr Groups
parse t = eval $ A.parse parser t
    where
        eval r =
            case r of
                Partial f  -> eval $ f ""
                Fail i c e -> Left $ errorMessage i c e
                Done "" v  -> Right v
                Done _  _  -> error "Should not happen"
        errorMessage :: Text -> [String] -> String -> String
        errorMessage i c e = e ++ " in (" ++ intercalate " > " c ++ ") near:\n" ++ unpack (T.takeWhile (not . isEndOfLine) i)

parser :: Parser Groups
parser = mapFromList <$> do
    comments
    manyTill group endOfInput <* endOfInput
    <?> "desktop entry"

group :: Parser (Text, Values)
group = do
    name <- header
    values <- many' keyValuePair
    return (name, HM.fromListWith (<>) values)
    <?> "group"

header :: Parser Text
header = do
    char '['
    identifier <- takeWhile1 notSquareBracket
    char ']'
    trailingSpaces
    return identifier
    <?> "header"
    where
        notSquareBracket '[' = False
        notSquareBracket ']' = False
        notSquareBracket _   = True

keyValuePair :: Parser (Text, Value)
keyValuePair = do
    k <- identifier   <?> "key"
    v <- parseValue k <?> "value"
    trailingSpaces
    return (k, v)
    <?> "key-value pair"

parseValue :: Text -> Parser Value
parseValue "Type"            = ascii singleString
parseValue "Version"         = ascii singleString
parseValue "Name"            = localized singleString
parseValue "GenericName"     = localized singleString
parseValue "NoDisplay"       = booleanValue
parseValue "Comment"         = localized singleString
parseValue "Icon"            = localized singleString
parseValue "Hidden"          = booleanValue
parseValue "OnlyShowIn"      = localized manyStrings
parseValue "NotShowIn"       = localized manyStrings
parseValue "DBusActivatable" = booleanValue
parseValue "TryExec"         = ascii singleString
parseValue "Exec"            = ascii singleString
parseValue "Path"            = ascii singleString
parseValue "Actions"         = ascii manyStrings
parseValue "Terminal"        = booleanValue
parseValue "MimeType"        = ascii manyStrings
parseValue "Categories"      = localized manyStrings
parseValue "Implements"      = ascii manyStrings
parseValue "Keywords"        = localized manyStrings
parseValue "StartupNotify"   = booleanValue
parseValue "StartupWMClass"  = ascii singleString
parseValue "URL"             = ascii singleString
parseValue _                 = booleanValue <|> ascii singleString <|> localized singleString

booleanValue :: Parser Value
booleanValue = do
    equals
    Boolean <$> choice [ string "true"  >> return True
                       , string "false" >> return False ]

localized :: (Bool -> Parser TextualValue) -> Parser Value
localized subType = do
    l <- option Default $ char '[' *> localeParser <* char ']'
    equals
    v <- subType True
    return . Textual . WithLocale . Localized $ HM.singleton l v

ascii :: (Bool -> Parser TextualValue)  -> Parser Value
ascii subType = do
    equals
    v <- subType False
    return $ Textual $ ASCII v

singleString :: Bool -> Parser TextualValue
singleString isUnicode = Single <$> escapedString False isUnicode

manyStrings :: Bool -> Parser TextualValue
manyStrings isUnicode = do
    strs <- many $ escapedString True isUnicode <* skipHorizontalSpace <* char ';'
    -- Something the last string isn't terminated with ';', accept it though it isn't canonical
    tailStr <- option Nothing $ Just <$> escapedString True isUnicode <* endOfLine
    return $ Many $ case tailStr of
        Nothing -> strs
        Just s  -> strs `snoc` s

escapedString :: Bool -> Bool -> Parser Text
escapedString isMany isUnicode =
    scan False go
    where
        go :: Bool -> Char -> Maybe Bool
        go False ';' | isMany    = Nothing
        go False '\\'            = Just True
        go True  _               = Just False
        go False c | isControl c = Nothing
                   | isUnicode   = Just False
                   | isAscii c   = Just False
                   | otherwise   = Nothing

identifier :: Parser Text
identifier = takeWhile1 (inClass "a-zA-Z0-9-") <?> "identifier"

equals :: Parser ()
equals = skipHorizontalSpace >> char '=' >> skipHorizontalSpace

trailingSpaces :: Parser ()
trailingSpaces = comments -- skipHorizontalSpace >> endOfLine >> comments

data CState = StartOfLine | Comment | Blank

comments :: Parser ()
comments = void $ scan StartOfLine test
    where
        test :: CState -> Char -> Maybe CState
        test _           c   | isEndOfLine c       = Just StartOfLine
        test Comment     _                         = Just Comment
        test StartOfLine '#'                       = Just Comment
        test Blank       '#'                       = Just Comment
        test _           c   | isHorizontalSpace c = Just Blank
                             | otherwise           = Nothing

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

class FromValue a where
    unwrapValue :: Value -> ErrorOr a

instance FromValue Bool where
    unwrapValue (Boolean b) = Right b
    unwrapValue _ = Left "Expected a boolean"

instance FromValue Text where
    unwrapValue (Textual (ASCII t)) = unwrapTextual t
    unwrapValue _ = Left "Expected a ASCII string"

instance FromValue [Text] where
    unwrapValue (Textual (ASCII t)) = unwrapTextual t
    unwrapValue _ = Left "Expected a list of ASCII string"

instance FromValue (Localized Text) where
    unwrapValue (Textual (WithLocale (Localized t))) = Localized <$> mapM unwrapTextual t
    unwrapValue _ = Left "Expected a localized string"

instance FromValue (Localized [Text]) where
    unwrapValue (Textual (WithLocale (Localized t))) = Localized <$> mapM unwrapTextual t
    unwrapValue _ = Left "Expected a list of localized string"

class FromTextualValue a where
    unwrapTextual :: TextualValue -> ErrorOr a

instance FromTextualValue Text where
    unwrapTextual (Single t) = Right t
    unwrapTextual _ = Left "Expected a single value"

instance FromTextualValue [Text] where
    unwrapTextual (Many t) = Right t
    unwrapTextual _ = Left "Expected a list"
