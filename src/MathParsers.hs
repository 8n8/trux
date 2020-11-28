module MathParsers where

import CommonParsers
    ( Id ( Id )
    , Numbered ( NumberOff, NumberOn )
    , Parser
    , parseCharFunc
    , parseFuncName
    , parseList
    , parseNumbered
    , parseTextContent
    , parseWhiteSpace
    , rowsSameLength
    )
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (intersperse)

math2Latex :: [MathElement] -> String
math2Latex = concatMap mathElement2Latex

mathElement2Latex :: MathElement -> String
mathElement2Latex mathElement = case mathElement of
    MathOrdinaryText text -> concat ["\\textrm{", text, "}"]
    MathBoldText text -> concat ["\\textbf{", text, "}" ]
    MathEnglishVar BoldMath var -> concat ["\\bm{", [var], "}"]
    MathEnglishVar ItalicMath var -> [var]
    MathOperatorChar operator -> [operator]
    Hollow character -> concat ["\\mathbb{", character:"}"]
    SimpleSubstitution latex -> latex
    Hat content -> concat ["\\hat{", mathElement2Latex content, "}"]
    Overline content -> concat ["\\overline{", mathElement2Latex content, "}"]
    Underline content -> concat ["\\underline{", mathElement2Latex content, "}"]
    OverArrow content -> concat ["\\vec{", mathElement2Latex content, "}"]
    MathNumbers numbers -> numbers
    GreekMath BoldMath greek -> concat ["\\bm{\\", greek, "}"]
    GreekMath ItalicMath greek -> "\\" ++ greek ++ " "
    Sqrt contents -> concat ["\\sqrt{", math2Latex contents, "}"]
    NthRoot n contents -> concat
        [ "\\sqrt["
        , mathElement2Latex n
        , "]{"
        , math2Latex contents
        , "}" ]
    Power contents -> concat ["^{", math2Latex contents, "}"]
    Fraction numerator denominator -> concat
        [ "\\cfrac{"
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
    AbsoluteBracket contents ->
        concat ["\\left|", math2Latex contents, "\\right|"]
    Condition contents ->
        concat ["\\condition{for $", math2Latex contents, "$}"]
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
    Matrix contents ->
        let
          rowLen = length . head $ contents
          alignments = take rowLen $ repeat 'c'
        in concat
          [ "\\begin{array}{"
          , alignments
          , "}"
          , concat $ intersperse " \\\\ " $ map matrixRow2Latex contents
          , "\\end{array}"
          ]

matrixRow2Latex :: [[MathElement]] -> String
matrixRow2Latex = concat . intersperse " & " . map matrixElement2Latex

matrixElement2Latex :: [MathElement] -> String
matrixElement2Latex = concatMap mathElement2Latex

derivDegree2Latex :: Char -> String
derivDegree2Latex '1' = ""
derivDegree2Latex degree = [ '[', degree, ']' ]


inlineMath2latex :: [MathElement] -> String
inlineMath2latex elements =
    concat [ "$", concatMap mathElement2Latex elements, "$" ]

displayMath2latex :: [DisplayMathLine] -> String
displayMath2latex equations = concat
    [ "\\begin{dgroup*}"
    , concatMap equation2latex equations
    , "\\end{dgroup*}" ]

math2latex :: Math -> String
math2latex (DisplayMath equations) = displayMath2latex equations
math2latex (InlineMath contents) = inlineMath2latex contents

data Math = DisplayMath [DisplayMathLine] | InlineMath [MathElement]
    deriving Show

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

data MathElement
  = MathOrdinaryText String
  | MathBoldText String
  | MathEnglishVar MathStyle Char
  | Hat MathElement
  | Overline MathElement
  | OverArrow MathElement
  | Underline MathElement
  | Hollow Char
  | SimpleSubstitution String
  | MathOperatorChar Char
  | MathNumbers String
  | GreekMath MathStyle String
  | Sqrt [MathElement]
  | NthRoot MathElement [MathElement]
  | Power [MathElement]
  | Fraction [MathElement] [MathElement]
  | Subscript [MathElement]
  | CurlyBracket [MathElement]
  | CurvedBracket [MathElement]
  | SquareBracket [MathElement]
  | AbsoluteBracket [MathElement]
  | PartialDerivative MathElement MathElement Char
  | OrdinaryDerivative MathElement MathElement Char
  | Condition [MathElement]
  | MixedPartialDerivative
        MathElement Char MathElement Char MathElement Char
  | Matrix [[[MathElement]]]
    deriving Show

data MathStyle = BoldMath | ItalicMath deriving Show

parseMathElement :: Parser MathElement
parseMathElement = choice
    [ parseMathOrdinaryText
    , parseMathBoldText
    , parseMathOperatorChar
    , parseHat
    , parseOverline
    , parseOverArrow
    , parseUnderline
    , parseCondition
    , parseMathHollowChar
    , parseMathEnglishVar
    , parseMathNumbers
    , parseGreekMath
    , parseSimpleSubstitution
    , parseSqrt
    , parseNthRoot
    , parsePower
    , parseFraction
    , parseSubscript
    , parseCurlyBracket
    , parseCurvedBracket
    , parseSquareBracket
    , parseAbsolute
    , parseOrdinaryDerivative
    , parsePartialDerivative
    , parseMixedPartialDerivative
    , parseMatrix
    ]

parseMathOrdinaryText :: Parser MathElement
parseMathOrdinaryText = fmap MathOrdinaryText parseTextContent

parseMathOperatorChar :: Parser MathElement
parseMathOperatorChar = do
    character <- choice $ fmap parseCharFunc mathOperatorChars
    return $ MathOperatorChar character

parseCondition :: Parser MathElement
parseCondition = do
    _ <- parseFuncName "for"
    content <- parseList '{' '}' parseMathElement
    return $ Condition content

parseMathHollowChar :: Parser MathElement
parseMathHollowChar =
    parseFuncName "hollow" >> Hollow <$> parseMathEnglishCapitals

parseMathEnglishVar :: Parser MathElement
parseMathEnglishVar = parseMathItalicChar <|> parseMathBoldChar

parseMathBoldChar :: Parser MathElement
parseMathBoldChar = try $ do
    _ <- try $ char '#'
    character <- parseMathEnglishChar
    return $ MathEnglishVar BoldMath character

parseMathBoldText :: Parser MathElement
parseMathBoldText = try $ do
    _ <- try $ char '#'
    txt <- parseTextContent
    return $ MathBoldText txt

parseMathItalicChar :: Parser MathElement
parseMathItalicChar = try $ do
    character <- parseMathEnglishChar
    return $ MathEnglishVar ItalicMath character

parseMathEnglishChar :: Parser Char
parseMathEnglishChar = try $ choice $ fmap parseCharFunc ordinaryMathChars
parseMathNumbers :: Parser MathElement
parseMathNumbers = do
    numbers <- try $ some $ oneOf "0123456789."
    parseWhiteSpace
    return $ MathNumbers numbers

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

parseSimpleSubstitution :: Parser MathElement
parseSimpleSubstitution =
    choice $ parseGeneralSimpleSub <$> simpleSubstitutions

parseGeneralSimpleSub :: (String, String) -> Parser MathElement
parseGeneralSimpleSub (truxcode, latexcode) =
    parseFuncName truxcode >> return (SimpleSubstitution latexcode)

simpleSubstitutions :: [(String, String)]
simpleSubstitutions =
    [ ("exp", "\\exp ")
    , ("int", "\\int ")
    , ("lim", "\\lim ")
    , ("infinity", "\\infty ")
    , ("-->", "\\rightarrow ")
    , ("==>", "\\Rightarrow ")
    , ("sum", "\\sum ")
    , ("log", "\\log ")
    , ("ln", "\\ln ")
    , ("sin", "\\sin ")
    , ("cos", "\\cos ")
    , ("tan", "\\tan ")
    , ("sinh", "\\sinh ")
    , ("cosh", "\\cosh ")
    , ("tanh", "\\tanh ")
    , ("dif", "\\dif ")
    , ("*", "\\times ")
    , ("star", "*")
    , ("<=", "\\leq ")
    , (">=", "\\geq ")
    , ("~=", "\\simeq ")
    , ("in", "\\in ")
    , ("notin", "\\notin ")
    , ("owns", "\\ni ")
    , ("subset", "\\subset ")
    , ("!=", "\\neq ")
    , ("subset=", "\\subseteq ")
    , ("superset", "\\supset ")
    , ("superset=", "\\supseteq ")
    , ("union", "\\cup ")
    , ("intersect", "\\cap ")
    , ("empty", "\\emptyset ")
    , ("diff", "\\setminus ")
    , ("mod", "\\mod ")
    , ("max", "\\max ")
    , ("min", "\\min ")
    , ("+-", "\\pm ")
    , ("curlyd", "\\partial ")
    , ("%", "\\% ")
    , ("deg", "^{\\circ} ")
    , ("dot", "\\cdot ")
    ]

parseSqrt :: Parser MathElement
parseSqrt = do
    _ <- parseFuncName "sqrt"
    content <- parseList '{' '}' parseMathElement
    return $ Sqrt content

parseNthRoot :: Parser MathElement
parseNthRoot = do
    _ <- parseFuncName "nthRoot"
    n <- parseMathElement
    content <- parseList '{' '}' parseMathElement
    return $ NthRoot n content

parseHat :: Parser MathElement
parseHat = parseFuncName "hat" >> Hat <$> parseMathElement

parseOverline :: Parser MathElement
parseOverline = parseFuncName "overline" >> Overline <$> parseMathElement

parseUnderline :: Parser MathElement
parseUnderline =
    parseFuncName "underline" >> Underline <$> parseMathElement

parseOverArrow :: Parser MathElement
parseOverArrow =
    parseFuncName "overArrow" >> OverArrow <$> parseMathElement

parsePower :: Parser MathElement
parsePower = parseShortPower <|> parseLongPower

parseFraction :: Parser MathElement
parseFraction = do
    numerator <- parseList '{' '}' parseMathElement
    _ <- parseFuncName "/"
    denominator <- parseList '{' '}' parseMathElement
    return $ Fraction numerator denominator

parseSubscript :: Parser MathElement
parseSubscript = parseShortSubscript <|> parseLongSubscript

parseMatrix :: Parser MathElement
parseMatrix = do
  _ <- parseFuncName "matrix"
  content <- parseList '{' '}' $ parseList '{' '}' $
    parseList '{' '}' parseMathElement
  if rowsSameLength content then
    return $ Matrix content
  else
    fail "Matrix rows must have equal lengths."

parseMatrixRow :: Parser [[MathElement]]
parseMatrixRow = parseList '{' '}' $ parseList '{' '}' parseMathElement

parseCurlyBracket :: Parser MathElement
parseCurlyBracket = do
    _ <- parseFuncName "curly"
    content <- parseList '{' '}' parseMathElement
    return $ CurlyBracket content

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

parseCurvedBracket :: Parser MathElement
parseCurvedBracket = CurvedBracket <$> parseList '(' ')' parseMathElement

parseSquareBracket :: Parser MathElement
parseSquareBracket = SquareBracket <$> parseList '[' ']' parseMathElement

parseAbsolute :: Parser MathElement
parseAbsolute = do
    _ <- parseFuncName "abs"
    AbsoluteBracket <$> parseList '{' '}' parseMathElement

parseOrdinaryDerivative :: Parser MathElement
parseOrdinaryDerivative = do
    _ <- parseFuncName "od"
    (ofvar, wrt, degree) <- parseDerivativeContents
    return $ OrdinaryDerivative ofvar wrt degree

parsePartialDerivative :: Parser MathElement
parsePartialDerivative = do
    _ <- parseFuncName "pd"
    (ofvar, wrt, degree) <- parseDerivativeContents
    return $ PartialDerivative ofvar wrt degree

parseDerivativeContents :: Parser (MathElement, MathElement, Char)
parseDerivativeContents = do
    ofvar <- parseGreekMath <|> parseMathEnglishVar
    wrt <- parseGreekMath <|> parseMathEnglishVar
    degree <- parseSingleDigit
    return (ofvar, wrt, degree)

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

parseGreekCharString :: Parser String
parseGreekCharString = choice $ fmap parseFuncName greekMathVars

greekMathVars :: [String]
greekMathVars =
    [ "alpha", "beta", "gamma", "Gamma", "delta", "Delta", "epsilon"
    , "zeta", "eta", "theta", "Theta", "iota", "lambda", "Lambda", "mu"
    , "nu", "xi", "Xi", "pi", "Pi", "rho", "sigma", "Sigma", "tau"
    , "upsilon", "Upsilon", "phi", "Phi", "chi", "psi", "Psi", "omega"
    , "Omega", "nabla"]

parseDisplayMath :: Parser Math
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

mathOperatorChars :: String
mathOperatorChars = "!=-+\'<>.,;:@\"/"

data DisplayMathLine = DisplayMathLine Numbered [MathElement] deriving Show

ordinaryMathChars :: String
ordinaryMathChars =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseMathEnglishCapitals :: Parser Char
parseMathEnglishCapitals =
    try $ choice $ parseCharFunc <$> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseInlineMathContent :: Parser [MathElement]
parseInlineMathContent = do
    _ <- parseFuncName "math"
    parseList '{' '}' parseMathElement

parseInlineMath :: Parser Math
parseInlineMath = fmap InlineMath parseInlineMathContent
