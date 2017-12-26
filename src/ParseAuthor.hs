module ParseAuthor ( parseAuthor ) where

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseAuthor :: Parser String
parseAuthor = try $ do
    _ <- string "author" 
    _ <- 


