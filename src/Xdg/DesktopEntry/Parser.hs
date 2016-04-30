{-# LANGUAGE NoImplicitPrelude #-}

module Xdg.DesktopEntry.Parser (
    Groups,
    Values,
    Value(..),
    parse
) where

import           ClassyPrelude          hiding (group)
import           Data.Attoparsec.Text   hiding (parse)
import qualified Data.HashMap.Strict    as HM
import           Data.Text              (strip)

import           Data.Locale
import           Xdg.DesktopEntry.Types

type Groups = HashMap Text Values
type Values = HashMap Text Value

data Value = StringValue Text
           | LocaleStringValue (HashMap Locale Text)
           | BoolValue Bool
           deriving (Eq, Show)

instance Semigroup Value where
    StringValue a        <> LocaleStringValue bs = LocaleStringValue (insertMap Default a bs)
    LocaleStringValue as <> StringValue b        = LocaleStringValue (insertMap Default b as)
    LocaleStringValue as <> LocaleStringValue bs = LocaleStringValue (as `union` bs)
    _                    <> b                    = b

parse :: Text -> Either String Groups
parse = parseOnly parser

parser :: Parser Groups
parser = mapFromList <$> manyTill group endOfInput <* endOfInput <?> "desktop entry"

group :: Parser (Text, Values)
group = do
    name <- header
    values <- many' keyValuePair
    return (name, HM.fromListWith (<>) values)
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
        notSquareBracket _   = True

keyValuePair :: Parser (Text, Value)
keyValuePair = do
    skipMany comment
    k <- identifier <?> "key"
    l <- optional $ char '[' *> localeParser <* char ']'
    skipHorizontalSpace
    char '='
    skipHorizontalSpace
    t <- strip <$> takeTill isEndOfLine <?> "value"
    skipHorizontalSpace
    endOfLine
    return (k, value l t)
    <?> "value line"
    where
        value Nothing  "true"  = BoolValue True
        value Nothing  "false" = BoolValue False
        value Nothing  x       = StringValue x
        value (Just l) x       = LocaleStringValue $ HM.singleton l x

identifier :: Parser Text
identifier = takeWhile1 (inClass "a-zA-Z0-9-") <?> "identifier"

comment :: Parser String
comment = choice [ char '#' *> manyTill anyChar endOfLine <?> "comment"
                 , manyTill space endOfLine               <?> "blank line" ]

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace
