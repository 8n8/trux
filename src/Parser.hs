module Parser ( parseDocument, wordChars ) where

import Control.Monad
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseDocument :: Parser String
parseDocument = try $ titleAndBody <|> onlyBody <|> onlyTitleInfo

titleAndBody :: Parser String
titleAndBody = try $ do
    titleinfo <- parseTitleInfo
    body <- parseBody
    return $ makeLatex (Just titleinfo) (Just body)

parseTitleInfo :: Parser String
parseTitleInfo = try $
    parseTitleAuthor <|> parseAuthorTitle
     
parseTitleAuthor :: Parser String
parseTitleAuthor = try $ do
    title <- parseTitle
    author <- parseAuthor
    return $ title ++ author

parseAuthorTitle :: Parser String
parseAuthorTitle = try $ do
    author <- parseAuthor
    title <- parseTitle
    return $ title ++ author

onlyTitleInfo :: Parser String
onlyTitleInfo = try $ do
    author <- parseAuthor
    title <- parseTitle
    eof
    return $ makeLatex (Just $ author ++ title) Nothing

onlyBody :: Parser String
onlyBody = try $ do
    body <- parseBody
    return $ makeLatex Nothing (Just body)

packages :: String
packages =
    "\\usepackage{bm}\
    \\\usepackage{amsmath}"

makeLatex :: Maybe String -> Maybe String -> String
makeLatex Nothing Nothing = ""
makeLatex Nothing (Just body) =
    "\\documentclass{article}\\begin{document}" ++ packages ++
        body ++ "\\end{document}"
makeLatex (Just titleinfo) Nothing =
    "\\documentclass{article}" ++ titleinfo ++ packages ++
        "\\begin{document}\\maketitle \\end{document}"
makeLatex (Just titleinfo) (Just body) =
    "\\documentclass{article}" ++ titleinfo ++ packages ++
        "\\begin{document}\\maketitle " ++ body ++ "\\end{document}"

parseBody :: Parser String
parseBody = fmap concat $ try $ do
    content <- try $ some $
        parseOrdinaryTextAndEnding <|> parseInlineMath <|> parseDisplayMath
    eof
    return content

parseOrdinaryTextAndEnding :: Parser String
parseOrdinaryTextAndEnding = try $ do
    content <- parseOrdinaryText
    void (try newline) <|> void (try $ char ' ') <|> void (try eof)
    return content

parseDisplayMath :: Parser String
parseDisplayMath = try $ do
    _ <- string "math"
    content <- parseMathEnvironment
    return $ "\\begin{align*}" ++ content ++ "\\end{align*}"

parseClosingCurlyBracket :: Parser ()
parseClosingCurlyBracket = try $ do
    _ <- try $ char '}'
    try $ void (try $ char ' ') <|> void (try newline) <|> try (lookAhead eof)

parseInlineMath :: Parser String
parseInlineMath = try $ do
    _ <- try $ char '$'
    content <- parseMathEnvironment
    return $ "$" ++ content ++ "$"

parseMathEnvironment :: Parser String
parseMathEnvironment = try $ do
    _ <- try $ char '{'
    void (try newline) <|> void (return "")
    content <- parseMathSymbols
    parseClosingCurlyBracket
    return content

parseMathSymbols :: Parser String
parseMathSymbols = fmap concat $ try $ some $ 
    parseMathCharAndEnding <|> parseSpecialMathSymbolAndEnding

parseSpecialMathSymbolAndEnding :: Parser String
parseSpecialMathSymbolAndEnding = try $ do
    symbol <- parseSpecialMathSymbol
    parseSpaceAndNoClosingCurly <|>
        void (try newline) <|> void (try $ lookAhead $ char '}')
    return symbol

parseSpecialMathSymbol :: Parser String
parseSpecialMathSymbol = try $
    parseIntegral <|>
    parsePower <|>
    parseSubscript <|>
    parseMathNumber <|>
    parseCurvedBrackets <|>
    parseSquareBrackets <|>
    parseCurlyBrackets <|>
    parseAngleBrackets <|>
    parseAbsolute <|>
    parseGreekMath <|>
    parseBoldVariable <|>
    parseMultiplication <|>
    parseMathText <|>
    parseDivision <|>
    parseTrig

parseDivision :: Parser String
parseDivision = try $ do
    _ <- string "div"
    return "\\div "

parseTrig :: Parser String
parseTrig = try $ do
    function <-
        string "sinh" <|>
        string "cosh" <|>
        string "tanh" <|>
        string "sin"  <|>
        string "cos" <|>
        string "log" <|>
        string "ln"
    return $ "\\" ++ function ++ " "

parseMathText :: Parser String
parseMathText = try $ do
    text <- parseOrdinaryText
    return $ "\\text{" ++ text ++ "}"

parseMultiplication :: Parser String
parseMultiplication = try $ do
    _ <- char '*'
    return "\\times "

parseBoldVariable :: Parser String
parseBoldVariable = try $ do
    void $ char 'b'
    var <- parseGreekMath <|> parseEnglishMath
    return $ "\\bm{" ++ var ++ "}"

parseEnglishMath :: Parser String
parseEnglishMath = try $ do
    c <- oneOf letters
    return [c]
  where
    letters :: String
    letters = 
        "abcdefghijklmnopqrstuvwxyz\
        \ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseGreekMath :: Parser String
parseGreekMath = try $ do
    letter <- string "alpha" <|>
              string "beta" <|>
              string "gamma" <|>
              string "Gamma" <|>
              string "delta" <|>
              string "Delta" <|>
              string "epsilon" <|>
              string "zeta" <|>
              string "eta" <|>
              string "theta" <|>
              string "Theta" <|>
              string "iota" <|>
              string "lambda" <|>
              string "Lambda" <|>
              string "mu" <|>
              string "nu" <|>
              string "xi" <|>
              string "Xi" <|>
              string "pi" <|>
              string "Pi" <|>
              string "rho" <|>
              string "sigma" <|>
              string "Sigma" <|>
              string "tau" <|>
              string "upsilon" <|>
              string "Upsilon" <|>
              string "phi" <|>
              string "Phi" <|>
              string "chi" <|>
              string "psi" <|>
              string "Psi" <|>
              string "omega" <|>
              string "Omega"
    return $ '\\':letter ++ " "

parseAbsolute :: Parser String
parseAbsolute = try $ do
    _ <- try $ char '|'
    _ <- try $ char '{'
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "\\left|" ++ content ++ "\\right|"

parseCurvedBrackets :: Parser String
parseCurvedBrackets = try $ do
    _ <- try $ char '('
    _ <- try $ char ' '
    content <- parseMathSymbols
    _ <- try $ char ')'
    return $ "\\left(" ++ content ++ "\\right)"

parseSquareBrackets :: Parser String
parseSquareBrackets = try $ do
    _ <- try $ char '['
    _ <- try $ char ' '
    content <- parseMathSymbols
    _ <- try $ char ']'
    return $ "\\left[" ++ content ++ "\\right]"

parseCurlyBrackets :: Parser String
parseCurlyBrackets = try $ do
    _ <- string "curly"
    _ <- try $ char '{'
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "\\left\\{" ++ content ++ "\\right\\}"

parseAngleBrackets :: Parser String
parseAngleBrackets = try $ do
    _ <- try $ string "angle"
    _ <- try $ char '{'
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "\\langle " ++ content ++ "\\rangle "

parseIntegral :: Parser String
parseIntegral = try $ do
    _ <- string "int"
    return "\\int "

parseMathNumber :: Parser String
parseMathNumber = try $ some $ digitChar <|> char '.'

parsePower :: Parser String
parsePower = try $ do
    _ <- try $ char '^'
    _ <- try $ char '{'
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "^{" ++ content ++ "}"

parseSubscript :: Parser String
parseSubscript = try $ do
    _ <- char '_'
    _ <- char '{'
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "_{" ++ content ++ "}"

parseSpaceAndNoClosingCurly :: Parser ()
parseSpaceAndNoClosingCurly = try $ do
    void $ try $ char ' '
    void $ try $ lookAhead $ notChar '}'

parseMathCharAndEnding :: Parser String
parseMathCharAndEnding = try $ do
    symbol <- parseSingleMathChar
    parseSpaceAndNoClosingCurly <|>
        void (try newline) <|> void (try $ lookAhead $ char '}') 
    return symbol
    
parseSingleMathChar :: Parser String
parseSingleMathChar = try $
    fmap (: []) (try $ oneOf singleCharMathSymbols) <|>
    parsePercentage <|>
    parseDollar <|>
    parseAmpersand

singleCharMathSymbols :: String
singleCharMathSymbols =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \|!£-=+/.><,"

parseOrdinaryText :: Parser String
parseOrdinaryText = try $ do
    void $ try $ char '`' 
    content <- parseTextQuoteContent
    void $ try $ char '`'
    -- void (try $ char ' ') <|> void (try newline) <|> try (lookAhead eof) <|> void (try $ lookAhead $ char '}')
    return content

parseTextQuoteContent :: Parser String
parseTextQuoteContent = fmap concat $ try $ some $ do
    whitespace1 <- parseWhitespace <|> parseEndQuoteNoConsume
    word <- parseWord
    whitespace2 <- parseWhitespace <|> parseEndQuoteNoConsume
    return $ whitespace1 ++ word ++ whitespace2

parseEndQuoteNoConsume :: Parser String
parseEndQuoteNoConsume = do
    void $ lookAhead $ try $ char '`'
    return "" 

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

parseWhitespace :: Parser String
parseWhitespace = try $
    parse2newlines <|>
    parse1newline <|>
    parse1space <|>
    return ""

-- parseNoWhiteSpace :: Parser String
-- parseNoWhiteSpace = return ""

parse1space :: Parser String
parse1space = try $ char ' ' >> return " "

parse1newline :: Parser String
parse1newline = try $ newline >> return "\n"

parse2newlines :: Parser String
parse2newlines = try $ do
    _ <- try newline
    _ <- try newline
    return "\n\n"

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

parseAuthor :: Parser String
parseAuthor = try $ do
    _ <- string "author" 
    content <- parsePreambleField
    return $ "\\author{" ++ content ++ "}"

parseTitle :: Parser String
parseTitle = try $ do
    _ <- string "title"
    content <- parsePreambleField
    return $ "\\title{" ++ content ++ "}"

parsePreambleField :: Parser String
parsePreambleField = try $ do
    _ <- try $ char '{'
    void (try newline) <|> void (return "")
    content <- parseOrdinaryText
    _ <- try $ char '}'
    void (try $ char ' ') <|> void (try newline) <|>
        void (try $ lookAhead eof)
    return content
