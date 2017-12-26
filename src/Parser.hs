-- | It provides a function for parsing a document and datatypes to 
-- represent it after parsing.

module Parser (parseDocument, Node(..), FunctionName(..) ) where

import Control.Monad ( void )
import Data.Void ( Void )
import Text.Megaparsec ( eof, lookAhead, Parsec, some, try, (<|>) )
import Text.Megaparsec.Char ( char, notChar, oneOf, string )

type Parser = Parsec Void String

-- | It is the basic component of the document, either an atom or a
-- function.  An atom is the smallest possible unit.  A function contains
-- one or more nodes as its arguments.
data Node = Atom String | Function FunctionName [Node] deriving (Show, Eq)

data Author = Author [AuthorChar]

-- | The built-in functions.
data FunctionName = 
    Author |
    DisplayMath |
    Document |
    InlineMath |
    Quote |
    Title
    deriving (Show, Eq)

-- | It parses the whole document.
parseDocument :: Parser [Node]
parseDocument = do
    nodes <- trySome parseNode
    eof
    return nodes

-- | It parses one node, which is either an atom or a function.
parseNode :: Parser Node
parseNode = parseAtom <|> parseFunction

-- | A non-consuming version of the 'some' function from Text.MegaParsec.
-- It keeps trying to parse a new thing of type 'a' till it fails, but
-- does not consume the thing that made it fail.
trySome :: Parser a -> Parser [a]
trySome = try . some

-- | Parses one function.
parseFunction :: Parser Node
parseFunction = try $ do
    funcName <- parseFunctionName
    nodes <- trySome parseNode
    parseCloseBracket
    return $ Function funcName nodes

-- | It parses the name of a function.  It can tell the difference between
-- a function name and an atom because a function name is followed by a 
-- space and an open curly bracket.
parseFunctionName :: Parser FunctionName
parseFunctionName = try $ do
    name <- parseFunctionKeyword
    parseOneSpace
    parseOpenBracket
    return name

-- | It tries to parse one of the built-in functions.  It tries one after
-- the other till one works or they all fail.
parseFunctionKeyword :: Parser FunctionName
parseFunctionKeyword = 
    parseFuncTitle <|>
    parseFuncAuthor <|>
    parseFuncQuote <|>
    parseFuncInlineMath <|>
    parseFuncDisplayMath

parseFuncTitle :: Parser FunctionName
parseFuncTitle = string "title" >> return Title

parseFuncAuthor :: Parser FunctionName
parseFuncAuthor = string "author" >> return Author

parseFuncQuote :: Parser FunctionName
parseFuncQuote = string "quote" >> return Quote

-- | It parses a character, but does not consume it if it fails.
tryChar :: Char -> Parser Char
tryChar = try . char

parseFuncInlineMath :: Parser FunctionName
parseFuncInlineMath = tryChar '$' >> return InlineMath

parseFuncDisplayMath :: Parser FunctionName
parseFuncDisplayMath = string "align" >> return DisplayMath

-- | An open bracket should always be followed by one space or newline.
parseOpenBracket :: Parser ()
parseOpenBracket = try $ void $ tryChar '{' >> parseOneSpace

-- | It parses the end of input but does not consume it if it fails.
tryEof :: Parser ()
tryEof = try eof

-- | A closing curly bracket should always be followed by a space, a
-- newline or the end of input.
parseCloseBracket :: Parser ()
parseCloseBracket = try $ tryChar '}' >> parseOneSpace <|> tryEof

-- | It parses one space or newline without consuming it.
parseOneSpace :: Parser ()
parseOneSpace = try $ void $ oneOf spaceChars 

-- | An atom is one of the basic units of the language.  It is one or more
-- of the standard English keyboard characters with a space or newline on
-- each side of it.
parseAtom :: Parser Node
parseAtom = try $ fmap Atom parseAtomString

-- | It is used to distinguish atoms from function names.  Function names
-- are followed by a space and an open curly bracket, atoms by a space and
-- a non-space non-curly-bracket character.
parseNoOpenBracket :: Parser ()
parseNoOpenBracket = lookAhead $ try $ void $ notChar '{'

-- | It parses the characters allowed in atoms till it fails, then a space
-- and then checks there is no open curly bracket.
parseAtomString :: Parser String
parseAtomString = try $ do
    atom <- trySome parseAtomChar
    parseOneSpaceAndNoOpenBracket <|> tryEof
    return atom

parseOneSpaceAndNoOpenBracket :: Parser ()
parseOneSpaceAndNoOpenBracket = try $ parseOneSpace >> parseNoOpenBracket

parseAtomChar :: Parser Char
parseAtomChar = try $ oneOf atomChars

-- | The set of characters that is allowed in an atom.
atomChars :: String
atomChars = 
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \01234567890\
    \`¬!\"£$%^&*()_-+=[];:'@#~,<.>/?\\|"

spaceChars :: String
spaceChars = " \n\r"
