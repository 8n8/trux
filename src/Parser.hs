module Parser ( parseDocument, combinedParser ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void ( Void )
import Control.Monad

type Parser = Parsec Void String

preamble :: String
preamble =
    "\\documentclass{article}\n\
    \\\begin{document}\n"

parseDocument :: Parser String
parseDocument = try $ do
    content <- fmap concat $ try $ some $ combinedParser
    return $ preamble ++ content ++ "\\end{document}"

combinedParser :: Parser String
combinedParser =
    parseDisplayMath <|>
    parseAuthor <|>
    parseOrdinaryText <|>
    parseInlineMath

parseDisplayMath :: Parser String
parseDisplayMath = dbg "parseDisplayMath" $ try $ do
    _ <- string "math"
    _ <- try $ char ' ' 
    _ <- try $ char '{'
    _ <- (try $ char ' ') <|> (try $ newline)
    content <- parseMathSymbols
    return $ "\\[" ++ content ++ "\\]"

parseInlineMath :: Parser String
parseInlineMath = dbg "parseInlineMath" $ try $ do
    _ <- try $ char '$'
    _ <- try $ char ' '
    _ <- try $ char '{'
    _ <- (try $ char ' ') <|> (try $ newline)
    content <- parseMathSymbols
    _ <- try $ char '}'
    (void $ try $ char ' ') <|> (void $ try $ newline) <|> (try eof)
    return $ "$" ++ content ++ "$"

parseMathSymbols :: Parser String
parseMathSymbols = fmap concat $ try $ some $ 
    parseMathCharAndEnding <|> parseSpecialMathSymbolAndEnding

parseSpecialMathSymbolAndEnding :: Parser String
parseSpecialMathSymbolAndEnding = try $ do
    symbol <- parseSpecialMathSymbol
    _ <- (try $ char ' ') <|> (try $ lookAhead $ char '}')
    return symbol

parseSpecialMathSymbol :: Parser String
parseSpecialMathSymbol = try $
    parseIntegral <|>
    parsePower <|>
    parseSubscript

parseIntegral :: Parser String
parseIntegral = try $ do
    _ <- string "int"
    return "\\int"

parsePower :: Parser String
parsePower = try $ do
    _ <- string "^{"
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "^{" ++ content ++ "}"

parseSubscript :: Parser String
parseSubscript = try $ do
    _ <- string "_{"
    content <- parseMathSymbols
    _ <- try $ char '}'
    return $ "_{" ++ content ++ "}"

parseMathCharAndEnding :: Parser String
parseMathCharAndEnding = try $ do
    symbol <- parseSingleMathChar
    _ <- (try $ char ' ') <|> (try $ lookAhead $ char '}')
    return symbol
    
parseSingleMathChar :: Parser String
parseSingleMathChar = try $
    (fmap (\x -> [x]) $ try $ oneOf singleCharMathSymbols) <|>
    parsePercentage <|>
    parseDollar

-- parseLeftCurvedBracket :: Parser String
-- parseLeftCurvedBracket = try $ do
--     _ <- char '('
--     return "\\left("
-- 
-- parseRightCurvedBracket :: Parser String
-- parseRightCurvedBracket = try $ do
--     _ <- char ')'
--     return "\\right)"
-- 
-- parseLeftSquareBracket :: Parser String
-- parseLeftSquareBracket = try $ do
--     _ <- char '['
--     return "\\left["
-- 
-- parseRightSquareBracket :: Parser String
-- parseRightSquareBracket = try $ do
--     _ <- char ']'
--     return "\\right]"

singleCharMathSymbols :: String
singleCharMathSymbols =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \!£$%-=+/.><,|"

parseOrdinaryText :: Parser String
parseOrdinaryText = dbg "parseOrdinaryText" $ fmap concat $ try $ some $ do
    word <- parseWord
    whitespace <- parseWhitespace <|> parseBracketAfterWord <|> parseEofAfterWord
    return $ word ++ whitespace

parseWord :: Parser String
parseWord = fmap concat $ try $ some $ do
    word <- parseWordChar <|> parseSpecialChar
    notFollowedBy $ lookAhead $ void $ string " { " 
    notFollowedBy $ lookAhead $ (void $ string " {") >> (void $ try $ eof) 
    notFollowedBy $ lookAhead $ (void $ string " {") >> (void $ try $ newline) 
    return word

parseEofAfterWord :: Parser String
parseEofAfterWord = try $ lookAhead $ eof >> return ""

parseBracketAfterWord :: Parser String
parseBracketAfterWord = try $ lookAhead $ char '}' >> return ""

parseWordChar :: Parser String
parseWordChar = fmap (\x -> [x]) $ oneOf wordChars

parseWhitespace :: Parser String
parseWhitespace = try $
    parse2newlines <|>
    parse1newline <|>
    parse1space

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
parseAuthor = dbg "parseAuthor" $ try $ do
    _ <- string "author" 
    _ <- try $ char ' '
    _ <- try $ char '{'
    (void $ try $ char ' ') <|> (void $ try newline)
    author <- try $ some $ (try $ oneOf authorChars) <|> parseAuthorSpace
    _ <- try $ char ' '
    _ <- try $ char '}'
    (void $ try $ char ' ') <|> (void $ try newline) <|> (void $ try eof)
    return $ "\\author{" ++ author ++ "}"

parseAuthorSpace :: Parser Char
parseAuthorSpace = try $ do
    _ <- try $ char ' '
    notFollowedBy $ lookAhead $ string "} "
    notFollowedBy $ lookAhead $ (try $ char '}') >> try eof
    notFollowedBy $ lookAhead $ (try $ char '}') >> newline
    return ' ' 

authorChars :: String
authorChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.'-"
