module Parser ( parseDocument, Document (..) ) where

import Control.Monad
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

bracket1 :: Char
bracket1 = '{'

bracket2 :: Char
bracket2 = '}'

doc2latex :: Document -> String
doc2latex (Document header body) =
    unlines [preamble, header2latex header, body2latex header body]

header2latex :: (Maybe DocHeader) -> String
header2latex Nothing = ""
header2latex (Just (DocHeader title author date)) =
    unlines [title2latex, author2latex, date2latex]

title2latex :: Title -> String
title2latex (Title headerElements) = concat
    [ "\\title{"
    , headerElements2Latex headerElements
    , "}" ]

headerElements2Latex :: [HeaderElement] -> String
headerElements2Latex = (fmap headerElement2Latex)

headerElement2Latex :: HeaderElement -> String
headerElement2Latex (HeaderMath mathElements) =
    mathElements2Latex mathElements
headerElements2Latex (HeaderText text) =

data Document = Document (Maybe DocHeader) DocumentBody deriving Show

data DocHeader = DocHeader Title (Maybe Author) (Maybe Date) deriving Show

newtype Date = Date [HeaderElement] deriving Show

newtype DocumentBody = DocumentBody [Element] deriving Show

data Element =
    Text String |
    DisplayMath [DisplayMathLine] |
    InlineMath [MathElement] |
    Header Numbered HeaderLevel [HeaderElement] |
    CrossReference Id
    deriving Show

newtype Title = Title [HeaderElement] deriving Show

newtype Author = Author [HeaderElement] deriving Show

parseDocument :: Parser Document
parseDocument = do
    header <- parseDocHeader
    body <- parseBody
    return $ Document header body

parseDate :: Parser (Maybe Date)
parseDate =
    (do
       _ <- parseFuncName "date"
       date <- parseList bracket1 bracket2 parseHeaderElement
       return $ Just $ Date date) <|>
    return Nothing

parseDocHeader :: Parser (Maybe DocHeader)
parseDocHeader =
    (do
        title <- parseTitle
        author <- parseAuthor
        date <- parseDate
        return $ Just $ DocHeader title author date) <|>
    return Nothing

parseBody :: Parser DocumentBody 
parseBody = do
    _ <- parseFuncName "body"
    body <- parseList bracket1 bracket2 parseElement
    return $ DocumentBody body

parseTitle :: Parser Title
parseTitle = do
    _ <- parseFuncName "title"
    title <- parseList bracket1 bracket2 parseHeaderElement
    return $ Title title

parseAuthor :: Parser (Maybe Author)
parseAuthor =
    (do
       _ <- parseFuncName "author"
       content <- parseList bracket1 bracket2 parseHeaderElement
       return $ Just $ Author content) <|>
    return Nothing

parseElement :: Parser Element
parseElement =
    parseText <|>
    parseDisplayMath <|>
    parseInlineMath <|>
    parseHeader <|>
    parseCrossReference

parseCrossReference :: Parser Element
parseCrossReference = do
    _ <- parseFuncName "ref"
    idCode <- parseId
    return $ CrossReference idCode

data HeaderLevel = HeaderOne | HeaderTwo | HeaderThree deriving Show

parseHeaderLevel :: Parser HeaderLevel
parseHeaderLevel =
    parseHeader1Level <|> parseHeader2Level <|> parseHeader3Level

parseHeader1Level :: Parser HeaderLevel
parseHeader1Level = do
    _ <- parseFuncName "1"
    return HeaderOne
 
parseHeader2Level :: Parser HeaderLevel
parseHeader2Level = do
    _ <- parseFuncName "2"
    return HeaderTwo

parseHeader3Level :: Parser HeaderLevel
parseHeader3Level = do
    _ <- parseFuncName "3"
    return HeaderThree

data HeaderElement =
    HeaderMath [MathElement] | HeaderText String
    deriving Show

parseHeader :: Parser Element
parseHeader = do
    _ <- parseFuncName "header"
    headerLevel <- parseHeaderLevel
    numbered <- parseNumbered
    header <- parseList bracket1 bracket2 parseHeaderElement
    return $ Header numbered headerLevel header

parseHeaderElement :: Parser HeaderElement
parseHeaderElement = parseMathInHeader <|> parseTextInHeader        

parseTextInHeader :: Parser HeaderElement
parseTextInHeader = do
    content <- parseTextContent
    return $ HeaderText content

parseMathInHeader :: Parser HeaderElement
parseMathInHeader = do
    content <- parseInlineMathContent
    return $ HeaderMath content

parseInlineMathContent :: Parser [MathElement]
parseInlineMathContent = do
    _ <- parseFuncName "math"
    parseList '{' '}' parseMathElement

parseInlineMath :: Parser Element
parseInlineMath = do
    content <- parseInlineMathContent
    return $ InlineMath content

data DisplayMathLine =
    DisplayMathLine Numbered [MathElement]
    deriving Show

data Numbered = NumberOn Id | NumberOff deriving Show

newtype Id = Id String deriving Show

data MathElement =
    MathOrdinaryText String |
    MathEnglishVar MathStyle Char |
    MathOperatorChar Char |
    MathNumbers String |
    GreekMath MathStyle String |
    Sqrt [MathElement] |
    Power [MathElement] |
    Fraction [MathElement] [MathElement] |
    Subscript [MathElement] |
    CurlyBracket [MathElement] |
    CurvedBracket [MathElement] |
    SquareBracket [MathElement] |
    SpecialMathSymbol String |
    AbsoluteBracket [MathElement]
    deriving Show

data MathStyle = BoldMath | ItalicMath deriving Show

parseWhiteSpace :: Parser ()
parseWhiteSpace = void (try $ char ' ') <|> void (try newline)

parseList :: (Show c) => Char -> Char -> Parser c -> Parser [c]
parseList startChar stopChar parseItem = do
    _ <- try $ char startChar
    parseWhiteSpace
    content <- try $ some $ do
        element <- parseItem
        return element
    _ <- try $ char stopChar
    parseWhiteSpace
    return content

parseFuncName :: String -> Parser String
parseFuncName name = try $ do
    _ <- string name
    parseWhiteSpace
    return name 

parseDisplayMath :: Parser Element
parseDisplayMath = do
    _ <- parseFuncName "Math"
    content <- parseList '{' '}' parseDisplayMathLine 
    return $ DisplayMath content

parseDisplayMathLine :: Parser DisplayMathLine
parseDisplayMathLine = do
    _ <- parseFuncName "equation"
    numbered <- parseNumbered
    content <- parseList '{' '}' parseMathElement
    return $ DisplayMathLine numbered content

parseMathElement :: Parser MathElement
parseMathElement =
    parseMathOrdinaryText <|>
    parseMathEnglishVar <|>
    parseMathOperatorChar <|>
    parseMathNumbers <|>
    parseGreekMath <|>
    parseSqrt <|>
    parsePower <|>
    parseFraction <|>
    parseSubScript <|>
    parseCurlyBracket <|>
    parseCurvedBracket <|>
    parseSquareBracket <|>
    parseSpecialMathSymbol <|>
    parseAbsolute


parseSpecialMathSymbol :: Parser MathElement
parseSpecialMathSymbol = do
    symbol <- choice $ fmap parseFuncName specialMathSymbols
    return $ SpecialMathSymbol symbol

specialMathSymbols :: [String]
specialMathSymbols = [
    "exp", "log", "ln", "sin", "cos", "tan", "sinh", "cosh", "tanh", "div"]

parseSquareBracket :: Parser MathElement
parseSquareBracket = do
    content <- parseList '[' ']' parseMathElement
    return $ SquareBracket content

parseCurvedBracket :: Parser MathElement
parseCurvedBracket = do
    content <- parseList '(' ')' parseMathElement
    return $ CurvedBracket content

parseCurlyBracket :: Parser MathElement
parseCurlyBracket = do
    _ <- parseFuncName "curly"
    content <- parseList '{' '}' parseMathElement
    return $ CurlyBracket content

parseFraction :: Parser MathElement
parseFraction = do
    numerator <- parseList '{' '}' parseMathElement
    _ <- parseFuncName "/"
    denominator <- parseList '{' '}' parseMathElement
    return $ Fraction numerator denominator

parseSubScript :: Parser MathElement
parseSubScript = do
    _ <- parseFuncName "_"
    content <- parseList '{' '}' parseMathElement
    return $ Subscript content

parsePower :: Parser MathElement
parsePower = do
    _ <- parseFuncName "^"
    content <- parseList '{' '}' parseMathElement
    return $ Power content

parseAbsolute :: Parser MathElement
parseAbsolute = do
    content <- parseList '|' '|' parseMathElement
    return $ AbsoluteBracket content

parseSqrt :: Parser MathElement
parseSqrt = do
    _ <- parseFuncName "sqrt"
    content <- parseList '{' '}' parseMathElement
    return $ Sqrt content

parseGreekMath :: Parser MathElement
parseGreekMath = parseBoldGreekMath <|> parseItalicGreekMath

parseBoldGreekMath :: Parser MathElement
parseBoldGreekMath = try $ do
    _ <- try $ char 'b'
    greekCharString <- parseGreekCharString
    return $ GreekMath BoldMath greekCharString

parseItalicGreekMath :: Parser MathElement
parseItalicGreekMath = try $ do
    greekCharString <- parseGreekCharString
    return $ GreekMath ItalicMath greekCharString

parseGreekCharString :: Parser String
parseGreekCharString = choice $ fmap parseFuncName greekMathVars

greekMathVars :: [String]
greekMathVars =
    [ "alpha", "beta", "gamma", "Gamma", "delta", "Delta", "epsilon"
    , "zeta", "eta", "theta", "Theta", "iota", "lambda", "Lambda" , "mu"
    , "nu", "xi", "Xi", "pi", "Pi", "rho", "sigma", "Sigma", "tau"
    , "upsilon", "Upsilon", "phi", "Phi", "chi", "psi", "Psi", "omega"
    , "Omega"]

parseMathNumbers :: Parser MathElement
parseMathNumbers = do
    numbers <- try $ some $ oneOf "0123456789."
    parseWhiteSpace
    return $ MathNumbers numbers

parseMathOperatorChar :: Parser MathElement
parseMathOperatorChar = do
    character <- choice $ fmap parseCharFunc mathOperatorChars
    return $ MathOperatorChar character

mathOperatorChars :: String
mathOperatorChars = "!=%*-+\'<>.,;:@\"/"

parseMathEnglishVar :: Parser MathElement
parseMathEnglishVar = parseMathItalicChar <|> parseMathBoldChar

parseMathItalicChar :: Parser MathElement
parseMathItalicChar = try $ do
    character <- parseMathEnglishChar
    return $ MathEnglishVar ItalicMath character

parseMathBoldChar :: Parser MathElement
parseMathBoldChar = try $ do
    _ <- try $ char 'b'
    character <- parseMathEnglishChar
    return $ MathEnglishVar BoldMath character

parseMathEnglishChar :: Parser Char
parseMathEnglishChar = try $ choice $ fmap parseCharFunc ordinaryMathChars

parseCharFunc :: Char -> Parser Char
parseCharFunc character = do
    _ <- try $ char character
    parseWhiteSpace
    return character
     
parseMathOrdinaryText :: Parser MathElement
parseMathOrdinaryText = do
    text <- parseTextContent
    return $ MathOrdinaryText text

ordinaryMathChars :: String
ordinaryMathChars =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseNumbered :: Parser Numbered
parseNumbered = parseNumberOn <|> return NumberOff

parseNumberOn :: Parser Numbered
parseNumberOn = do
    _ <- parseFuncName "num"
    idCode <- parseId
    return $ NumberOn idCode

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

parseText :: Parser Element
parseText = fmap Text parseTextContent

parseTextContent :: Parser String
parseTextContent = try $ do
    void $ try $ char '`' 
    beginningWhitespace <- string " " <|> return ""
    mainContent <- fmap concat $ try $ some $ do
        word <- parseWord
        whiteSpace <- string " " <|> return ""
        return $ word ++ whiteSpace
    void $ try $ char '`'
    parseWhiteSpace
    return $ beginningWhitespace ++ mainContent

parseWord :: Parser String
parseWord = try $ do
    openQuote <- parseOpenQuote
    word <- some $ parseWordChar <|> parseSpecialChar
    return $ openQuote ++ concat word

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

parseSpecialChar :: Parser String
parseSpecialChar = try $
    parseDollar <|>
    parsePercentage <|>
    parseUnderscore <|>
    parseTilde <|>
    parseBackslash <|>
    parseAmpersand

parseAmpersand :: Parser String
parseAmpersand = try $ do
    _ <- char '&'
    return "\\&"

parseDollar :: Parser String
parseDollar = try $ do
    _ <- char '$'
    return "\\$"

parsePercentage :: Parser String
parsePercentage = try $ do
    _ <- char '%'
    return "\\%"

parseUnderscore :: Parser String
parseUnderscore = try $ do
    _ <- char '_'
    return "\\_"

-- REMEMBER TO INCLUDE textcomp PACKAGE IN PREAMBLE
parseTilde :: Parser String
parseTilde = try $ do
    _ <- char '~'
    return "\\textasciitilde "

parseBackslash :: Parser String
parseBackslash = try $ do
    _ <- char '\\'
    return "\\textbackslash "
