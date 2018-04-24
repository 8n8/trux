module CommonParsers where

import Control.Monad
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

rowsSameLength :: [[a]] -> Bool
rowsSameLength [] = True
rowsSameLength (a:b) =
  let
    firstLen = length a
  in
    all ((== firstLen) . length) b

parseTextContent :: Parser String
parseTextContent = do
    _ <- try $ char '`'
    beginningWhitespace <- string " " <|> return ""
    mainContent <- fmap concat $ some $ do
        word <- parseWord
        whiteSpace <- string " " <|> return ""
        return $ word ++ whiteSpace
    _ <- try $ char '`'
    parseWhiteSpace
    return $ beginningWhitespace ++ mainContent

parseOneWhiteSpace :: Parser ()
parseOneWhiteSpace = void (try $ char ' ') <|> void (try newline)

parseWhiteSpace :: Parser ()
parseWhiteSpace = void $ some parseOneWhiteSpace

parseWord :: Parser String
parseWord = try $ do
    openQuote <- parseOpenQuote
    word <- some $ parseWordChar <|> parseSpecialChar
    return $ openQuote ++ concat word

parseSpecialChar :: Parser String
parseSpecialChar = try $ choice
    [ parseDollar
    , parsePercentage
    , parseUnderscore
    , parseTilde
    , parseBackslash
    , parseAmpersand ]

parseOpenQuote :: Parser String
parseOpenQuote = try $
    (try (char '\"') >> return "``") <|>
    (try (char '\'') >> return "`") <|>
    return ""

parseWordChar :: Parser String
parseWordChar = (: []) <$> oneOf wordChars

wordChars :: String
wordChars =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \01234567890\
    \!\"£*()-+=[];:'@#,<.>/?|"

parseAmpersand :: Parser String
parseAmpersand = try (char '&') >> return "\\&"

parseDecimal :: Parser String
parseDecimal = try $ do
    _ <- try $ char '.'
    _ <- notFollowedBy parseWhiteSpace
    return "."

parseDollar :: Parser String
parseDollar = try (char '$') >> return "\\$"

parsePercentage :: Parser String
parsePercentage = try (char '%') >> return "\\%"

parseUnderscore :: Parser String
parseUnderscore = try (char '_') >> return "\\_"

parseTilde :: Parser String
parseTilde = try (char '_') >> return "\\textasciitilde "

parseBackslash :: Parser String
parseBackslash = try (char '\\') >> return "\\textbackslash "

parseList :: (Show c) => Char -> Char -> Parser c -> Parser [c]
parseList startChar stopChar parseItem = do
    _ <- try $ char startChar
    parseWhiteSpace
    content <- try $ many parseItem
    _ <- try $ char stopChar
    parseWhiteSpace
    return content

parseFuncName :: String -> Parser String
parseFuncName name = try $ string name >> parseWhiteSpace >> return name

data Numbered = NumberOn Id | NumberOff deriving Show

newtype Id = Id String deriving Show

parseNumbered :: Parser Numbered
parseNumbered = parseNumberOn <|> return NumberOff

parseNumberOn :: Parser Numbered
parseNumberOn = parseFuncName "num" >> fmap NumberOn parseId

parseCharFunc :: Char -> Parser Char
parseCharFunc character = try $ do
    _ <- try $ char character
    parseWhiteSpace
    return character

parseId :: Parser Id
parseId = do
    idCode <- try $ some $ oneOf idChars
    parseWhiteSpace
    return $ Id idCode

idChars :: String
idChars =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \0123456789"
