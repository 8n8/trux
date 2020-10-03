module Parser ( parse2Latex ) where

import CommonParsers
    ( Id ( Id )
    , Numbered (NumberOff, NumberOn)
    , Parser
    , parseCharFunc
    , parseFuncName
    , parseId
    , parseList
    , parseNumbered
    , parseTextContent
    , rowsSameLength
    )
import Data.List (intersperse, transpose, findIndices, sort)
import MathParsers
    ( parseInlineMath
    , Math
    , math2latex
    , parseDisplayMath )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)

bracket1 :: Char
bracket1 = '{'

bracket2 :: Char
bracket2 = '}'

preamble :: String
preamble =
    "\\documentclass{article}\n\
    \\\usepackage[utf8]{inputenc}\n\
    \\\usepackage{microtype}\n\
    \\\usepackage{listings}\n\
    \\\usepackage[hidelinks]{hyperref}\n\
    \\\usepackage{amsmath}\n\
    \\\usepackage{amsfonts}\n\
    \\\usepackage[backend=biber,style=authoryear]{biblatex}\n\
    \\\bibliography{ref}\n\
    \\\usepackage{graphicx}\n\
    \\\usepackage{bm}\n\
    \\\usepackage{float}\n\
    \\\usepackage{textcomp}\n\
    \\\usepackage{commath}\n\
    \\\usepackage{siunitx}\n\
    \\\usepackage[official]{eurosym}\n\
    \\\frenchspacing\n\
    \\\usepackage{booktabs}\n\
    \\\usepackage{breqn}\n\
    \\\usepackage{cleveref}\n\
    \\\creflabelformat{equation}{#2#1#3}\n\
    \\\crefdefaultlabelformat{#2#1#3}\n\
    \\\lstset{basicstyle=\\ttfamily}\n"

parse2Latex :: Parser String
parse2Latex = fmap doc2latex parseDocument

doc2latex :: Document -> String
doc2latex (Document header body) =
    unlines [preamble, docHeader2latex header, body2latex header body]

body2latex :: Maybe DocHeader -> DocumentBody -> String
body2latex header (DocumentBody elements) =
    unlines
      [ "\\begin{document}"
      , maketitle
      , body
      , "\\printbibliography \\end{document}"
      ]
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
    TableOfContents -> "\\tableofcontents "
    Italic text -> concat ["\\textit{", text, "}"]
    Url text -> concat ["\\url{", text, "}"]
    Hyperlink visible link -> concat ["\\href{", link, "}", "{", visible, "}" ]
    Bold text -> concat ["\\textbf{", text, "}"]
    CodeFromFile Nothing filePath ->
        concat ["\\lstinputlisting{", filePath, "}"]
    CodeFromFile (Just (Language lang)) filePath ->
        concat ["\\lstinputlisting[language=", lang, "]{", filePath, "}" ]
    Monospace text -> concat ["\\texttt{", text, "}"]
    Footnote elements -> footnote2latex elements
    MathElement contents -> math2latex contents
    BulletPoints bullets -> concat
        [ "\n\\begin{itemize}"
        , bullets2latex bullets
        , "\n\\end{itemize}"
        ]
    NumberedList bullets -> concat
        [ "\n\\begin{enumerate}"
        , list2latex bullets
        , "\n\\end{enumerate}"
        ]
    Header numbered level elements -> header2latex numbered level elements
    CrossReference (Id idCode) -> concat [ "\\cref{", idCode, "}" ]
    Umlaut character -> ['\\', '\"', character]
    ElementSimpleSub symbol -> symbol
    Table (Id idCode) caption contents -> concat
        [ "\n\\begin{table}[H]\\centering \\begin{tabular}{"
        , tableAlignStr contents
        , "}\\toprule "
        , tableHeader2Latex $ head contents
        , " \\\\\\midrule "
        , concat $ intersperse " \\\\ " $ map tableRow2Latex $ tail contents
        , "\\\\\\bottomrule \\end{tabular} \\caption{"
        , concatMap element2latex caption
        , "} \\label{"
        , idCode
        , "} \\end{table}"
        ]
    Image (Id idCode) width caption filename -> concat
        [ "\\begin{figure}[H]\\centering \\includegraphics[width="
        , show width
        , "\\textwidth]{"
        , filename
        , "} \\caption{"
        , concatMap element2latex caption
        , "} \\label{"
        , idCode
        , "} \\end{figure}"
        ]
    Citation (Id idCode) -> concat [ "\\cite{", idCode, "}" ]

list2latex :: [NumberedPoint] -> String
list2latex bullets = concatMap listItem2latex bullets

bullets2latex :: [[Element]] -> String
bullets2latex = concatMap bullet2latex 

bullet2latex :: [Element] -> String
bullet2latex elements = concat
    [ "\n\\item "
    , concatMap element2latex elements
    ]

listItem2latex :: NumberedPoint -> String
listItem2latex (NumberedPoint Nothing elements) = concat
    [ "\n\\item "
    , concatMap element2latex elements
    ]
listItem2latex (NumberedPoint (Just (Id idCode)) elements) = concat
    [ "\n\\item "
    , " \\label{"
    , idCode
    , "} "
    , concatMap element2latex elements
    ]

tableAlignStr :: [[[Element]]] -> String
tableAlignStr table =
  let
    numIndices = sort $ numColIndices table
    lenRow = length $ head table
  in
    concat $ take lenRow $ map (insertDecimalAlign numIndices) [0..]

insertDecimalAlign :: [Int] -> Int -> String
insertDecimalAlign numIndices i =
  if elem i numIndices then
    "S[table-text-alignment=left]"
  else
    "l"

numColIndices :: [[[Element]]] -> [Int]
numColIndices =
  findIndices (== True) . map (and . map isNum) . transpose . tail

isNum :: [Element] -> Bool
isNum [Text str] =
  case readMaybe str :: Maybe Float of
    Nothing -> False
    Just _ -> True
isNum _ = False

tableHeader2Latex :: [[Element]] -> String
tableHeader2Latex row = concat
  [ "{"
  , concat . intersperse "} & {" . map tableElement2Latex $ row
  , "}"
  ]

tableRow2Latex :: [[Element]] -> String
tableRow2Latex = concat . intersperse " & " . map tableElement2Latex

tableElement2Latex :: [Element] -> String
tableElement2Latex = concatMap element2latex


footnote2latex :: [Element] -> String
footnote2latex elements = concat
    [ "\\footnote{"
    , concatMap element2latex elements
    , "}"
    ]

header2latex :: Numbered -> HeaderLevel -> [Element] -> String
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
    , concatMap element2latex elements
    , "}"
    , case numbered of
        NumberOn (Id idCode) -> concat [ "\\label{", idCode, "}" ]
        NumberOff -> ""
    ]

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
date2latex Nothing = "\\date{}"

headerElements2Latex :: [Element] -> String
headerElements2Latex = concatMap element2latex

data Document = Document (Maybe DocHeader) DocumentBody deriving Show

data DocHeader = DocHeader Title (Maybe Author) (Maybe Date) deriving Show

newtype Date = Date [Element] deriving Show

newtype DocumentBody = DocumentBody [Element] deriving Show

data NumberedPoint = NumberedPoint (Maybe Id) [Element] deriving Show

data Element
  = Text String
  | Italic String
  | Bold String
  | Monospace String
  | MathElement Math
  | Header Numbered HeaderLevel [Element]
  | BulletPoints [[Element]]
  | NumberedList [NumberedPoint]
  | CrossReference Id
  | Citation Id
  | Umlaut Char
  | ElementSimpleSub String
  | Table Id [Element] [[[Element]]]
  | Image Id Float [Element] String
  | Footnote [Element]
  | CodeFromFile (Maybe Language) FilePath
  | Url String
  | Hyperlink String String
  | TableOfContents
    deriving Show

newtype Language = Language String deriving Show

newtype Title = Title [Element] deriving Show

newtype Author = Author [Element] deriving Show

parseElementSimpleSub :: Parser Element
parseElementSimpleSub =
    choice $ parseGeneralElementSimpleSub <$> simpleElementSubs

parseGeneralElementSimpleSub :: (String, String) -> Parser Element
parseGeneralElementSimpleSub (truxcode, latexcode) =
    parseFuncName truxcode >> return (ElementSimpleSub latexcode)

simpleElementSubs :: [(String, String)]
simpleElementSubs =
    [ ("euro", "\\euro{}")
    , ("p", "\n\n")
    ]

parseUmlaut :: Parser Element
parseUmlaut = do
    _ <- try $ char '\"'
    character <- choice $ parseCharFunc <$>
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    return $ Umlaut character

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
    _ <- eof
    return $ DocumentBody body

parseTitle :: Parser Title
parseTitle = do
    _ <- parseFuncName "title"
    title <- parseList bracket1 bracket2 parseHeaderElement
    return $ Title title

parseCode :: Parser Element
parseCode = do
    _ <- parseFuncName "code"
    lang <- fmap Just parseCodeLang <|> return Nothing
    filepath <- parseTextContent
    return $ CodeFromFile lang filepath

parseCodeLang :: Parser Language
parseCodeLang = do
    _ <- parseFuncName "lang"
    lang <- parseTextContent
    return $ Language lang

parseAuthor :: Parser (Maybe Author)
parseAuthor =
    (do
       _ <- parseFuncName "author"
       content <- parseList bracket1 bracket2 parseHeaderElement
       return $ Just $ Author content) <|>
    return Nothing

parseElement :: Parser Element
parseElement = choice
    [ parseText
    , parseItalic
    , parseTableOfContents
    , parseCode
    , parseMonospace
    , MathElement <$> parseDisplayMath
    , MathElement <$> parseInlineMath
    , parseHeader
    , parseCrossReference
    , parseCitation
    , parseElementSimpleSub
    , parseUmlaut
    , parseTable
    , parseImage
    , parseFootnote
    , parseBold
    , parseHyperlink
    , parseUrl
    , parseBulletPoints
    , parseNumberedList
    ]

parseTableOfContents :: Parser Element
parseTableOfContents = do
  _ <- parseFuncName "tableofcontents"
  return TableOfContents

parseItalic :: Parser Element
parseItalic = do
  _ <- parseFuncName "i"
  fmap Italic parseTextContent

parseHyperlink :: Parser Element
parseHyperlink = do
    _ <- parseFuncName "link"
    visible <- parseTextContent
    link <- parseTextContent
    return $ Hyperlink visible link

parseUrl :: Parser Element
parseUrl = do
    _ <- parseFuncName "url"
    fmap Url parseTextContent

parseBold :: Parser Element
parseBold = do
    _ <- parseFuncName "b"
    fmap Bold parseTextContent

parseMonospace :: Parser Element
parseMonospace = do
  _ <- parseFuncName "t"
  fmap Monospace parseTextContent

parseCitation :: Parser Element
parseCitation = do
  _ <- parseFuncName "cite"
  idCode <- parseId
  return $ Citation idCode

parseCrossReference :: Parser Element
parseCrossReference = do
    _ <- parseFuncName "ref"
    idCode <- parseId
    return $ CrossReference idCode

data HeaderLevel = HeaderOne | HeaderTwo | HeaderThree deriving Show

parseImage :: Parser Element
parseImage = do
  _ <- parseFuncName "image"
  idCode <- parseId
  widthStr <- parseTextContent
  case readMaybe widthStr :: Maybe Float of
    Nothing -> fail
      "The width argument to 'image' must be a number between 0 and 1."
    Just width -> do
      caption <- parseList '{' '}' parseHeaderElement
      filename <- parseTextContent
      return $ Image idCode width caption filename

parseTable :: Parser Element
parseTable = do
  _ <- parseFuncName "table"
  idCode <- parseId
  caption <- parseList '{' '}' parseHeaderElement
  content <- parseList '{' '}' $ parseList '{' '}' $
    parseList '{' '}' parseElement
  if rowsSameLength content then
    if length content > 1 then
      return $ Table idCode caption content
    else
      fail "Table must have at least two rows."
  else
    fail "Table rows must have equal length."

parseHeaderLevel :: Parser HeaderLevel
parseHeaderLevel =
    parseHeader1Level <|> parseHeader2Level <|> parseHeader3Level

parseHeader1Level :: Parser HeaderLevel
parseHeader1Level = parseFuncName "1" >> return HeaderOne

parseHeader2Level :: Parser HeaderLevel
parseHeader2Level = parseFuncName "2" >> return HeaderTwo

parseHeader3Level :: Parser HeaderLevel
parseHeader3Level = parseFuncName "3" >> return HeaderThree

parseHeader :: Parser Element
parseHeader = do
    _ <- parseFuncName "header"
    headerLevel <- parseHeaderLevel
    numbered <- parseNumbered
    header <- parseList bracket1 bracket2 parseHeaderElement
    return $ Header numbered headerLevel header

parseMaybeId :: Parser (Maybe Id)
parseMaybeId =
  (Just <$> parseId) <|> (return Nothing)

parseBulletPoints :: Parser Element
parseBulletPoints = do
  _ <- parseFuncName "bullets"
  BulletPoints <$> (parseList '{' '}' parseBullet)

parseNumberedList :: Parser Element
parseNumberedList = do
  _ <- parseFuncName "list"
  NumberedList <$> (parseList '{' '}' parseListItem)

parseBullet :: Parser [Element]
parseBullet = parseList '{' '}' parseFootnoteElement

parseListItem :: Parser NumberedPoint
parseListItem = do
  maybeId <- parseMaybeId
  content <- parseList '{' '}' parseFootnoteElement
  return $ NumberedPoint maybeId content

parseFootnote :: Parser Element
parseFootnote = do
    _ <- parseFuncName "footnote"
    footnote <- parseList bracket1 bracket2 parseFootnoteElement
    return $ Footnote footnote

parseFootnoteElement :: Parser Element
parseFootnoteElement = choice
    [ parseText
    , MathElement <$> parseInlineMath
    , MathElement <$> parseDisplayMath
    , parseCrossReference
    , parseItalic
    , parseMonospace
    , parseBold
    , parseElementSimpleSub
    , parseUmlaut ]

parseHeaderElement :: Parser Element
parseHeaderElement = choice
    [ parseText
    , MathElement <$> parseInlineMath
    , parseCrossReference
    , parseBold
    , parseMonospace
    , parseGeneralElementSimpleSub ("euro", "\\euro{}")
    , parseUmlaut ]

parseText :: Parser Element
parseText = fmap Text parseTextContent
