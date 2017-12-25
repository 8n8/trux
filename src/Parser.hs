module Parser (parseDocument, Node(..), FunctionName(..) ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

data Node = Atom String | Function FunctionName [Node] deriving (Show, Eq)

data FunctionName = 
    Document |
    Title |
    Author |
    Quote |
    InlineMath |
    DisplayMath deriving (Show, Eq)

parseDocument :: Parser [Node]
parseDocument = do
    nodes <- trySome parseNode
    _ <- eof
    return nodes

parseNode :: Parser Node
parseNode = parseSymbol <|> parseFunction

trySome :: Parser a -> Parser [a]
trySome = try . some

parseFunction :: Parser Node
parseFunction = try consumingParseFunction

consumingParseFunction :: Parser Node
consumingParseFunction = do
    funcName <- parseFunctionName
    nodes <- trySome parseNode
    _ <- parseCloseBracket
    return $ Function funcName nodes

parseFunctionName :: Parser FunctionName
parseFunctionName = try consumingFuncNameParser

consumingFuncNameParser :: Parser FunctionName
consumingFuncNameParser =
    parseFuncTitle <|>
    parseFuncAuthor <|>
    parseFuncQuote <|>
    parseFuncInlineMath <|>
    parseFuncDisplayMath >>= \name ->
    parseOneSpace >>
    parseOpenBracket >>
    return name

parseFuncTitle :: Parser FunctionName
parseFuncTitle = string "title" >> return Title

parseFuncAuthor :: Parser FunctionName
parseFuncAuthor = string "author" >> return Author

parseFuncQuote :: Parser FunctionName
parseFuncQuote = string "quote" >> return Quote

tryChar :: Char -> Parser Char
tryChar c = try . char $ c

parseFuncInlineMath :: Parser FunctionName
parseFuncInlineMath =  tryChar '$' >> return InlineMath

parseFuncDisplayMath :: Parser FunctionName
parseFuncDisplayMath = string "align" >> return DisplayMath

parseOpenBracket :: Parser ()
parseOpenBracket = try (tryChar '{' >> parseOneSpace >> return ())

tryEof :: Parser ()
tryEof = try eof

parseCloseBracket :: Parser ()
parseCloseBracket = try ( 
    tryChar '}' >>
    parseOneSpace <|> tryEof >>
    return ())

parseOneSpace :: Parser ()
parseOneSpace = do
    _ <- try (oneOf spaceChars)
    return ()

parseSymbol :: Parser Node
parseSymbol = try ( do
    symbolString <- parseSymbolString
    return $ Atom symbolString)

parseNoOpenBracket :: Parser ()
parseNoOpenBracket = lookAhead (try (notChar '{') >> return ())

parseSymbolString :: Parser String
parseSymbolString = try (
    trySome parseSymbolChar >>= \symbol ->
    parseOneSpaceAndNoOpenBracket <|> tryEof >>
    return symbol)

parseOneSpaceAndNoOpenBracket :: Parser ()
parseOneSpaceAndNoOpenBracket = try (parseOneSpace >> parseNoOpenBracket)

parseSymbolChar :: Parser Char
parseSymbolChar = try (oneOf symbolChars)

symbolChars :: String
symbolChars = 
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \01234567890\
    \`¬!\"£$%^&*()_-+=[];:'@#~,<.>/?\\|"

spaceChars :: String
spaceChars = " \n\r"
