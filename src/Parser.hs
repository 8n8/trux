module Parser ( parse2Latex ) where

import Control.Monad
import Data.Map as Map
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

bracket1 :: Char
bracket1 = '{'

bracket2 :: Char
bracket2 = '}'

preamble :: String
preamble =
    "\\documentclass{article}\n\
    \\\usepackage[utf8]{inputenc}\n\
    \\\usepackage{amsmath}\n\
    \\\usepackage{bm}\n\
    \\\usepackage{textcomp}\n\
    \\\usepackage{commath}\n\
    \\\usepackage{breqn}\n"

parse2Latex :: Parser String
parse2Latex = fmap doc2latex parseDocument

doc2latex :: Document -> String
doc2latex (Document header body) =
    unlines [preamble, docHeader2latex header, body2latex header body]

body2latex :: Maybe DocHeader -> DocumentBody -> String
body2latex header (DocumentBody elements) =
    unlines ["\\begin{document}", maketitle, body, "\\end{document}"]
  where
    body :: String
    body = concatMap element2latex elements
    maketitle :: String
    maketitle =case header of
        Just _ -> "\\maketitle "
        Nothing -> ""

element2latex :: Element -> String
element2latex element = case element of
    Text text -> text
    DisplayMath equations -> displayMath2latex equations
    InlineMath contents -> inlineMath2latex contents
    Header numbered level elements -> header2latex numbered level elements 
    CrossReference (Id idCode) -> concat [ "\\ref{", idCode, "}" ]

header2latex :: Numbered -> HeaderLevel -> [HeaderElement] -> String
header2latex numbered level elements = concat
    [ "\\"
    , case level of
        HeaderOne -> "section"
        HeaderTwo -> "subsection"
        HeaderThree -> "subsubsection"
    , case numbered of
        NumberOn _ -> ""
        NumberOff -> "*"
    , "{"
    , headerElements2Latex elements
    , "}"
    , case numbered of
        NumberOn (Id idCode) -> concat [ "\\label{", idCode, "}" ]
        NumberOff -> ""
    ]

inlineMath2latex :: [MathElement] -> String
inlineMath2latex elements =
    concat [ "$", concatMap mathElement2Latex elements, "$" ]

displayMath2latex :: [DisplayMathLine] -> String
displayMath2latex equations = concat
    [ "\\begin{dgroup*}"
    , concatMap equation2latex equations
    , "\\end{dgroup*}" ]

equation2latex :: DisplayMathLine -> String
equation2latex (DisplayMathLine numbered elements) = case numbered of
    NumberOn (Id idCode) -> concat
        [ "\\begin{dmath}"
        , mathContents
        , "\\label{"
        , idCode
        , "}"
        , "\\end{dmath}" ]
    NumberOff -> concat
        [ "\\begin{dmath*}"
        , mathContents
        , "\\end{dmath*}" ]
  where
    mathContents = concatMap mathElement2Latex elements
    
docHeader2latex :: Maybe DocHeader -> String
docHeader2latex Nothing = ""
docHeader2latex (Just (DocHeader title author date)) =
    unlines [title2latex title, author2latex author, date2latex date]

author2latex :: Maybe Author -> String
author2latex (Just (Author headerElements)) = concat
    [ "\\author{"
    , headerElements2Latex headerElements
    , "}" ]
author2latex Nothing = ""

title2latex :: Title -> String
title2latex (Title headerElements) = concat
    [ "\\title{"
    , headerElements2Latex headerElements
    , "}" ]

date2latex :: Maybe Date -> String
date2latex (Just (Date headerElements)) = concat
    [ "\\date{"
    , headerElements2Latex headerElements
    , "}" ]
date2latex Nothing = ""

headerElements2Latex :: [HeaderElement] -> String
headerElements2Latex = concatMap headerElement2Latex

headerElement2Latex :: HeaderElement -> String
headerElement2Latex (HeaderMath mathElements) =
    concat [ "$", math2Latex mathElements, "$" ]
headerElement2Latex (HeaderText text) = text

math2Latex :: [MathElement] -> String
math2Latex = concatMap mathElement2Latex

mathElement2Latex :: MathElement -> String
mathElement2Latex mathElement = case mathElement of
    MathOrdinaryText text -> concat ["\\textrm{", text, "}"] 
    MathEnglishVar BoldMath var -> concat ["\\bm{", [var], "}"]
    MathEnglishVar ItalicMath var -> [var]
    MathOperatorChar operator -> [operator]
    SimpleSubstitution latex -> latex
    MathNumbers numbers -> numbers
    GreekMath BoldMath greek -> concat ["\\bm{\\", greek, "}"]
    GreekMath ItalicMath greek -> "\\" ++ greek
    Sqrt contents -> concat ["\\sqrt{", math2Latex contents, "}"]
    Power contents -> concat ["^{", math2Latex contents, "}"]
    Fraction numerator denominator -> concat
        [ "\\dfrac{"
        , math2Latex numerator
        , "}{"
        , math2Latex denominator
        , "}" ]
    Subscript contents -> concat ["_{", math2Latex contents, "}"]
    CurlyBracket contents ->
        concat ["\\left\\{", math2Latex contents, "\\right\\}"]
    CurvedBracket contents ->
        concat ["\\left(", math2Latex contents, "\\right)"]
    SquareBracket contents ->
        concat ["\\left[", math2Latex contents, "\\right]"]
    SpecialMathSymbol symbol -> concat ["\\", symbol, " "]
    AbsoluteBracket contents ->
        concat ["\\left|", math2Latex contents, "\\right|"]
    Multiplication -> "\\times "
    Star -> "*"
    Integral contents wrt -> concat
        [ "\\int "
        , math2Latex contents
        , "\\dif "
        , mathElement2Latex wrt ]
    Differential -> "\\dif "
    OrdinaryDerivative ofvar wrt degree -> concat
        [ "\\dod"
        , derivDegree2Latex degree
        , "{"
        , mathElement2Latex ofvar
        , "}{"
        , mathElement2Latex wrt
        , "}" ]
    PartialDerivative ofvar wrt degree -> concat
        [ "\\dpd"
        , derivDegree2Latex degree
        , "{"
        , mathElement2Latex ofvar
        , "}{"
        , mathElement2Latex wrt
        , "}" ]
    MixedPartialDerivative wrt1 degree1 wrt2 degree2 wrt3 degree3 -> concat
        [ "\\dmd{"
        , mathElement2Latex wrt1
        , "}{"
        , [degree1]
        , "}{"
        , mathElement2Latex wrt2
        , "}{"
        , [degree2]
        , "}{"
        , mathElement2Latex wrt3
        , "}{"
        , [degree3]
        , "}" ]
    LessThanOrEqualTo -> "\\leq "
    MoreThanOrEqualTo -> "\\geq "
    ApproximatelyEqualTo -> "\\simeq "

derivDegree2Latex :: Char -> String
derivDegree2Latex '1' = ""
derivDegree2Latex degree = [ '[', degree, ']' ]

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
parseHeader1Level = parseFuncName "1" >> return HeaderOne
 
parseHeader2Level :: Parser HeaderLevel
parseHeader2Level = parseFuncName "2" >> return HeaderTwo

parseHeader3Level :: Parser HeaderLevel
parseHeader3Level = parseFuncName "3" >> return HeaderThree

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
parseTextInHeader = fmap HeaderText parseTextContent

parseMathInHeader :: Parser HeaderElement
parseMathInHeader = fmap HeaderMath parseInlineMathContent

parseInlineMathContent :: Parser [MathElement]
parseInlineMathContent = do
    _ <- parseFuncName "math"
    parseList '{' '}' parseMathElement

parseInlineMath :: Parser Element
parseInlineMath = fmap InlineMath parseInlineMathContent

data DisplayMathLine = DisplayMathLine Numbered [MathElement] deriving Show

data Numbered = NumberOn Id | NumberOff deriving Show

newtype Id = Id String deriving Show

data MathElement =
    MathOrdinaryText String |
    MathEnglishVar MathStyle Char |
    Hollow Char |
    SimpleSubstitution String |
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
    AbsoluteBracket [MathElement] |
    Multiplication |
    Star |
    Integral [MathElement] MathElement |
    Differential |
    PartialDerivative MathElement MathElement Char |
    OrdinaryDerivative MathElement MathElement Char |
    MixedPartialDerivative
        MathElement Char MathElement Char MathElement Char |
    LessThanOrEqualTo |
    MoreThanOrEqualTo |
    ApproximatelyEqualTo |
    SetSymbols Set
    deriving Show

data Set = O | N | Z | Q | A | R | C | H | O | S | In | NotIn | Owns |
    Subset | SubsetEqual | Superset | SupersetEqual | Cup | Cap | Diff

simpleSymbols :: Map.Map String MathElement
simpleSymbols = [ "nat", "integer", "real", "in", "notin", "owns"
    , "subset", "subset=", "supset", "supset=", "cup", "cap", "setdiff" ]

data MathStyle = BoldMath | ItalicMath deriving Show

parseDifferential :: Parser MathElement
parseDifferential = parseFuncName "dif" >> return Differential 

parseIntegral :: Parser MathElement
parseIntegral = do
    _ <- parseFuncName "int"
    content <- parseList '{' '}' parseMathElement
    wrt <- parseMathElement
    return $ Integral content wrt

parseWhiteSpace :: Parser ()
parseWhiteSpace = void (try $ char ' ') <|> void (try newline)

parseList :: (Show c) => Char -> Char -> Parser c -> Parser [c]
parseList startChar stopChar parseItem = do
    _ <- try $ char startChar
    parseWhiteSpace
    content <- try $ some parseItem
    _ <- try $ char stopChar
    parseWhiteSpace
    return content

parseFuncName :: String -> Parser String
parseFuncName name = try $ string name >> parseWhiteSpace >> return name

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
parseMathElement = try $ choice
    [ parseMathOrdinaryText
    , parseMathOperatorChar
    , parseMathEnglishVar
    , parseMathNumbers
    , parseGreekMath
    , parseSqrt
    , parsePower
    , parseFraction
    , parseSubscript
    , parseCurlyBracket
    , parseCurvedBracket
    , parseSquareBracket
    , parseSpecialMathSymbol
    , parseAbsolute
    , parseMultiplication
    , parseStar
    , parseOrdinaryDerivative
    , parsePartialDerivative
    , parseMixedPartialDerivative
    , parseIntegral
    , parseDifferential
    , parseLessThanOrEqualTo
    , parseMoreThanOrEqualTo
    , parseApproximatelyEqualTo ]

parseApproximatelyEqualTo :: Parser MathElement
parseApproximatelyEqualTo =
    parseFuncName "~=" >> return ApproximatelyEqualTo

parseLessThanOrEqualTo :: Parser MathElement
parseLessThanOrEqualTo = parseFuncName "<=" >> return LessThanOrEqualTo

parseMoreThanOrEqualTo :: Parser MathElement
parseMoreThanOrEqualTo = parseFuncName ">=" >> return MoreThanOrEqualTo

parseMultiplication :: Parser MathElement
parseMultiplication = parseFuncName "*" >> return Multiplication

parseSpecialMathSymbol :: Parser MathElement
parseSpecialMathSymbol =
    SpecialMathSymbol <$> choice (parseFuncName <$> specialMathSymbols)

specialMathSymbols :: [String]
specialMathSymbols = [
    "exp", "log", "ln", "sin", "cos", "tan", "sinh", "cosh", "tanh", "div"]

parseSquareBracket :: Parser MathElement
parseSquareBracket = SquareBracket <$> parseList '[' ']' parseMathElement

parseCurvedBracket :: Parser MathElement
parseCurvedBracket = CurvedBracket <$> parseList '(' ')' parseMathElement

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

parseSubscript :: Parser MathElement
parseSubscript = parseShortSubscript <|> parseLongSubscript

parseShortSubscript :: Parser MathElement
parseShortSubscript = try $ do
    _ <- try $ char '_'
    element <- parseMathElement
    return $ Subscript [element]

parseLongSubscript :: Parser MathElement
parseLongSubscript = do
    _ <- parseFuncName "_"
    content <- parseList '{' '}' parseMathElement
    case content of
        [_] -> fail "For subscripts containing only one element, use \
            \_element notation, such as x _3."
        _ -> return $ Subscript content

parsePower :: Parser MathElement
parsePower = parseShortPower <|> parseLongPower

parseShortPower :: Parser MathElement
parseShortPower = try $ do
    _ <- try $ char '^'
    element <- parseMathElement
    return $ Power [element]

parseLongPower :: Parser MathElement
parseLongPower = do
    _ <- parseFuncName "^"
    content <- parseList '{' '}' parseMathElement
    case content of
        [_] -> fail "For powers containing only one element, use ^element \
            \notation, such as e ^x."
        _ -> return $ Power content

parseDerivativeContents :: Parser (MathElement, MathElement, Char)
parseDerivativeContents = do
    ofvar <- parseGreekMath <|> parseMathEnglishVar
    wrt <- parseGreekMath <|> parseMathEnglishVar
    degree <- parseSingleDigit
    return (ofvar, wrt, degree)

parsePartialDerivative :: Parser MathElement
parsePartialDerivative = do
    _ <- parseFuncName "pd"
    (ofvar, wrt, degree) <- parseDerivativeContents
    return $ PartialDerivative ofvar wrt degree

parseOrdinaryDerivative :: Parser MathElement
parseOrdinaryDerivative = do
    _ <- parseFuncName "od"
    (ofvar, wrt, degree) <- parseDerivativeContents
    return $ OrdinaryDerivative ofvar wrt degree

parseMixedPartialDerivative :: Parser MathElement
parseMixedPartialDerivative = do
    _ <- parseFuncName "pdMix"
    wrt1 <- parseGreekMath <|> parseMathEnglishVar
    degree1 <- parseSingleDigit
    wrt2 <- parseGreekMath <|> parseMathEnglishVar
    degree2 <- parseSingleDigit
    wrt3 <- parseGreekMath <|> parseMathEnglishVar
    degree3 <- parseSingleDigit
    return $ MixedPartialDerivative wrt1 degree1 wrt2 degree2 wrt3 degree3

parseSingleDigit :: Parser Char
parseSingleDigit = do
    d <- try digitChar
    _ <- parseWhiteSpace
    return d

parseAbsolute :: Parser MathElement
parseAbsolute = AbsoluteBracket <$> parseList '|' '|' parseMathElement

parseSqrt :: Parser MathElement
parseSqrt = do
    _ <- parseFuncName "sqrt"
    content <- parseList '{' '}' parseMathElement
    return $ Sqrt content

parseGreekMath :: Parser MathElement
parseGreekMath = parseBoldGreekMath <|> parseItalicGreekMath

parseBoldGreekMath :: Parser MathElement
parseBoldGreekMath = try $ do
    _ <- try $ char '#'
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

parseStar :: Parser MathElement
parseStar = parseFuncName "star" >> return Star

mathOperatorChars :: String
mathOperatorChars = "!=%-+\'<>.,;:@\"/"

parseMathEnglishVar :: Parser MathElement
parseMathEnglishVar = parseMathItalicChar <|> parseMathBoldChar

parseMathItalicChar :: Parser MathElement
parseMathItalicChar = try $ do
    character <- parseMathEnglishChar
    return $ MathEnglishVar ItalicMath character

parseMathHollowChar :: Parser MathElement
parseMathHollowChar = Hollow <$> parseMathEnglishCapitals

parseMathBoldChar :: Parser MathElement
parseMathBoldChar = try $ do
    _ <- try $ char '#'
    character <- parseMathEnglishChar
    return $ MathEnglishVar BoldMath character

parseMathEnglishChar :: Parser Char
parseMathEnglishChar = try $ choice $ fmap parseCharFunc ordinaryMathChars

parseMathEnglishCapitals :: Parser Char
parseMathEnglishCapitals =
    try $ choice $ parseCharFunc <$> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseCharFunc :: Char -> Parser Char
parseCharFunc character = try $ do
    _ <- try $ char character
    parseWhiteSpace
    return character
     
parseMathOrdinaryText :: Parser MathElement
parseMathOrdinaryText = fmap MathOrdinaryText parseTextContent

ordinaryMathChars :: String
ordinaryMathChars =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseNumbered :: Parser Numbered
parseNumbered = parseNumberOn <|> return NumberOff

parseNumberOn :: Parser Numbered
parseNumberOn = parseFuncName "num" >> fmap NumberOn parseId

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
    _ <- try $ char '`' 
    beginningWhitespace <- string " " <|> return ""
    mainContent <- fmap concat $ try $ some $ do
        word <- parseWord
        whiteSpace <- string " " <|> return ""
        return $ word ++ whiteSpace
    _ <- try $ char '`'
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
parseSpecialChar = try $ choice
    [ parseDollar
    , parsePercentage
    , parseUnderscore
    , parseTilde
    , parseBackslash
    , parseAmpersand ]

parseAmpersand :: Parser String
parseAmpersand = try (char '&') >> return "\\&"

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
