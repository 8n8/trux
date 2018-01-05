module Parser ( parseDocument, wordChars ) where

import Control.Monad
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseDocument :: Parser String
parseDocument = titleAndBody <|> onlyBody <|> onlyTitleInfo

titleAndBody :: Parser String
titleAndBody = do
    titleinfo <- parseTitleInfo
    body <- parseBody
    return $ makeLatex (Just titleinfo) (Just body)

parseTitleInfo :: Parser String
parseTitleInfo = parseTitleAuthor <|> parseAuthorTitle
     
parseTitleAuthor :: Parser String
parseTitleAuthor = do
    title <- parseTitle
    author <- parseAuthor
    return $ title ++ author

parseAuthorTitle :: Parser String
parseAuthorTitle = do
    author <- parseAuthor
    title <- parseTitle
    return $ title ++ author

onlyTitleInfo :: Parser String
onlyTitleInfo = do
    author <- parseAuthor
    title <- parseTitle
    eof
    return $ makeLatex (Just $ author ++ title) Nothing

onlyBody :: Parser String
onlyBody = do
    body <- parseBody
    return $ makeLatex Nothing (Just body)

packages :: String
packages =
    "\\usepackage{bm}\
    \\\usepackage{amsmath}\
    \\\usepackage{breqn}"

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
parseBody = fmap concat $ do
    content <- some $
        parseDisplayMath <|>
        parseOrdinaryTextAndEnding <|>
        parseInlineMath <|>
        parseHeader1 <|>
        parseHeader2 <|>
        parseHeader3
    eof
    return content

parseHeader3 :: Parser String
parseHeader3 = do
    star <- (string "header3num" >> return "") <|>
        (string "header3" >> return "*")
    _ <- try $ char '{'
    content <- fmap concat $ some $
        parseOrdinaryTextAndEnding <|> parseInlineMath
    parseClosingCurlyBracket
    return $ "\\subsubsection" ++ star ++ "{" ++ content ++ "}"

parseHeader2 :: Parser String
parseHeader2 = do
    star <- (string "header2num" >> return "") <|>
        (string "header2" >> return "*")
    _ <- try $ char '{'
    content <- fmap concat $ some $
        parseOrdinaryTextAndEnding <|> parseInlineMath
    parseClosingCurlyBracket
    return $ "\\subsection" ++ star ++ "{" ++ content ++ "}"

parseHeader1 :: Parser String
parseHeader1 = do
    star <- (string "header1num" >> return "") <|>
        (string "header1" >> return "*")
    _ <- try $ char '{'
    content <- fmap concat $ some $
        parseOrdinaryTextAndEnding <|> parseInlineMath
    parseClosingCurlyBracket
    return $ "\\section" ++ star ++ "{" ++ content ++ "}"

parseOrdinaryTextAndEnding :: Parser String
parseOrdinaryTextAndEnding = do
    content <- parseOrdinaryText
    parseNewlineSpaceCloseCurlyOrEof
    return content

parseNewlineSpaceCloseCurlyOrEof :: Parser ()
parseNewlineSpaceCloseCurlyOrEof =
    void (try newline) <|>
    void (try $ char ' ') <|>
    void (try eof) <|>
    void (try $ lookAhead $ char '}')

parseDisplayMath :: Parser String
parseDisplayMath = do
    _ <- string "math"
    _ <- try $ char '{'
    void (try newline) <|> return ()
    content <- fmap concat $ try $ some parseOneLineOfDisplayMath
    parseClosingCurlyBracket
    return $ "\\begin{dgroup*}" ++ content ++ "\\end{dgroup*}"

parseClosingCurlyBracket :: Parser ()
parseClosingCurlyBracket = try $ do
    _ <- try $ char '}'
    try $ (void (try $ char ' ') >>
            void (try $ lookAhead $ notChar '}'))  <|>
        void (try newline) <|> try (lookAhead eof) <|>
        void (try $ lookAhead $ char '}')

parseInlineMath :: Parser String
parseInlineMath = do
    _ <- try $ char '$'
    _ <- try $ char '{'
    void (try newline) <|> void (return "")
    content <- parseMathSymbols
    parseClosingCurlyBracket
    return $ "$" ++ content ++ "$"

parseOneLineOfDisplayMath :: Parser String
parseOneLineOfDisplayMath = do
    content <- parseMathSymbols
    star <- parseEquationNumber <|>  parseNoEquationNumber
    parseSemiColonAndSpace <|> lookAhead parseClosingCurlyBracket
    return $
        "\\begin{dmath" ++ star ++ "}" ++ content ++
            "\\end{dmath" ++ star ++ "}"

parseNoEquationNumber :: Parser String
parseNoEquationNumber = try $ do
    (void $ try $ lookAhead $ char ';') <|>
        (void $ try $ lookAhead $ char '}')
    return "*" 

parseEquationNumber :: Parser String
parseEquationNumber = try $ do
    _ <- string "num"
    ((void $ try $ char ' ') >> (void $ try $ lookAhead $ char ';')) <|>
        (void $ try $ lookAhead $ char '}')
    return "" 

parseSemiColonAndSpace :: Parser ()
parseSemiColonAndSpace = try $ do
    _ <- try $ char ';'
    (void $ try $ char ' ') <|> (void $ try newline)
    
parseMathSymbols :: Parser String
parseMathSymbols = fmap concat $ some $ 
    parseMathCharAndEnding <|>
    parseSpecialMathSymbolAndEnding

parseSpecialMathSymbolAndEnding :: Parser String
parseSpecialMathSymbolAndEnding = try $ do
    symbol <- parseSpecialMathSymbolCommon
    parseSpaceAndNoClosingCurly <|>
        void (try newline) <|> void (try $ lookAhead $ char '}')
    return symbol

parseSpecialMathSymbolCommon :: Parser String
parseSpecialMathSymbolCommon = try $
    parseIntegral <|>
    parsePower <|>
    parseSubscript <|>
    parseSquareRoot <|>
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
        string "exp" <|>
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
parseMathNumber = try $ some $ oneOf digits

digits :: String
digits = "0123456789."

parsePower :: Parser String
parsePower = try $ do
    _ <- try $ char '^'
    _ <- try $ char '{'
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "^{" ++ content ++ "}"

parseSquareRoot :: Parser String
parseSquareRoot = try $ do
    _ <- string "sqrt"
    _ <- try $ char '{'
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "\\sqrt{" ++ content ++ "}"

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
    \=|!£-+/.><,"

parseOrdinaryText :: Parser String
parseOrdinaryText = try $ do
    void $ try $ char '`' 
    content <- parseTextQuoteContent
    void $ try $ char '`'
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
