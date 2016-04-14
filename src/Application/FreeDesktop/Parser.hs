{-# LANGUAGE OverloadedStrings #-}

module Application.FreeDesktop.Parser (
    parseDesktopEntry,
    lookupGroup,
    lookupText,
    lookupBool,
    lookupNumber
) where

import           Data.Attoparsec.Text as DAT
import qualified Data.Char            as DC
import           Data.Text            (Text, unpack)
import qualified Data.Text.IO         as DTI

parseDesktopEntry :: Text -> Maybe DesktopEntry
parseDesktopEntry content =
    case parseOnly desktopEntry content of
        Left  _ -> Nothing
        Right x -> Just x

lookupGroup :: Text -> DesktopEntry -> Maybe Group
lookupGroup = lookup

lookupText :: Text -> Group -> Maybe Text
lookupText = lookupValue asText
    where asText (String s) = Just s
          asText _          = Nothing

lookupBool :: Text -> Group -> Maybe Bool
lookupBool = lookupValue asBool
    where asBool (Bool b) = Just b
          asBool _        = Nothing

lookupNumber :: Text -> Group -> Maybe Double
lookupNumber = lookupValue asNumber
    where asNumber (Number n) = Just n
          asNumber _          = Nothing

lookupValue :: (Value -> Maybe a) -> Text -> Group -> Maybe a
lookupValue f key group = lookup (Key key Nothing) group >>= f

type DesktopEntry = [(Text, Group)]

type Group = [(Key, Value)]

data Key = Key { key :: Text, locale :: Maybe Locale }
    deriving (Ord, Eq)

instance Show Key where
    show (Key key Nothing) = concat ["Key \"", unpack key, "\""]
    show (Key key (Just locale)) = concat ["Key \"", unpack key, "[", show locale, "]\""]

data Locale = Locale {
       lang     :: Text,
       country  :: Maybe Text,
       encoding :: Maybe Text,
       modifier :: Maybe Text
    }
    deriving (Ord, Eq)

instance Show Locale where
    show (Locale l c e m) = concat [unpack l, country c, encoding e, modifier m]
                           where
                              country Nothing = ""
                              country (Just c) = '_' : unpack c
                              encoding Nothing = ""
                              encoding (Just e) = '.' : unpack e
                              modifier Nothing = ""
                              modifier (Just m) = '@' : unpack m

data Value = String Text
             | Bool Bool
             | Number Double
    deriving (Show, Ord, Eq)

desktopEntry :: Parser DesktopEntry
desktopEntry = manyTill group endOfInput <* endOfInput <?> "desktop entry"

group :: Parser (Text, Group)
group = do
    key <- header
    values <- many' keyValuePair
    return (key, values)
    <?> "group"

header :: Parser Text
header = do
    skipMany comment
    char '['
    identifier <- takeWhile1 notSquareBracket
    char ']'
    skipHorizontalSpace
    endOfLine
    return identifier
    <?> "header"
    where
        notSquareBracket '[' = False
        notSquareBracket ']' = False
        notSquareBracket _ = True

keyValuePair :: Parser (Key, Value)
keyValuePair = do
    skipMany comment
    key <- identifier <?> "key"
    locale <- optional (char '[' *> localeSpec <* char ']')
    skipHorizontalSpace
    char '='
    skipHorizontalSpace
    value <- value <?> "value"
    skipHorizontalSpace
    endOfLine
    return (Key key locale, value)
    <?> "value line"

localeSpec :: Parser Locale
localeSpec = do
    lang <- identifier <?> "lang"
    country <- optional (char '_' *> identifier) <?> "country"
    encoding <- optional (char '.' *> identifier) <?> "encoding"
    modifier <- optional (char '@' *> identifier) <?> "modifier"
    return $ Locale lang country encoding modifier
    <?> "locale"

optional :: Parser a -> Parser (Maybe a)
optional p = option Nothing (fmap Just p)

identifier :: Parser Text
identifier = DAT.takeWhile1 (inClass "a-zA-Z0-9-") <?> "identifier"

value :: Parser Value
value = choice [
            string "true"  >> return (Bool True),
            string "false" >> return (Bool False),
            fmap Number double,
            fmap String (takeTill isEndOfLine)
        ]

comment :: Parser String
comment = choice [
            char '#' *> manyTill anyChar endOfLine <?> "comment",
            manyTill space endOfLine               <?> "blank line"
          ]

skipHorizontalSpace = skipWhile isHorizontalSpace
