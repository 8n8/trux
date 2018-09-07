\documentclass[12pt, a4paper]{scrartcl}
\usepackage{listings}
\usepackage{booktabs}
\usepackage{microtype}
\usepackage{hyperref}
\usepackage{cleveref}
\usepackage[backend=biber]{biblatex}
\bibliography{references.bib}
\usepackage{xcolor}
\hypersetup{
    colorlinks,
    linkcolor={red!50!black},
    citecolor={blue!50!black},
    urlcolor={blue!80!black}
}

\title{Trux}
\author{True Ghiassi}
\begin{document}
\maketitle
Trux is a wrapper language around Latex, intended for simple and easy creation of attractive technical documents.  The problem I have with Latex for writing technical documents, especially at work, is that I find it time-consuming and difficult to use, even after many years experience of it.  So I often end up using Word or something that looks inferior, simply because of needing to get something done quickly.  

When existing users encourage people to use Latex, they often say that you will be able to concentrate on content and just let Latex handle the appearance.  My experience with Latex is that I spend an enormous amount of time fiddling with it to make it look nice.  For example, to make a simple table with numbers in it that align at the decimal points involves importing several packages, and doing a lot of fiddling around and looking things up online.  Trux hides all this away, allowing you to get on with the work quickly.

Another thing that Trux tries hard to improve is the error messages.  I find it really hard to debug Latex code.  Often the only way is to comment it out line by line to find the error.  Trux has really precise and clear error messages.  Most of the time it will tell you exactly what went wrong and where.

The reason for inventing a whole new markup language when there are loads already around (like Latex, the various flavours of markdown etc) is that it is really important for technical documents to be able to enter complex mathematics easily.  Mostly, markup languages that support mathematics require you to enter chunks of Latex, and I find Latex really annoying for entering anything but really simple mathematics.  Trux tries to make it really easy and logical to enter advanced mathematics.

A trux document is contained in a plain-text file called \textless some-name\textgreater .tx.  References are contained in an ordinary bibtex file called ref.bib.  It is run from the command-line with \texttt{trux \textless some-name\textgreater .tx}.  This will run trux to generate a Latex file, and then run Latex the right number of times to handle all the cross-references and citations, generate a pdf of the final document, and then delete all the junk files that Latex generates.

The program is written in Haskell, partly because it is what I know and like, but also because I thought a purely functional language is very well adapted to making a compiler.  It takes an input file, passes it through a bunch of pure functions and produces the Latex file.

\section{The main function}

@d funcMain @{
main :: IO ()
main = do
    filepath <- fmap head getArgs
    filecontents <- readFile filePath
    let fileRoot = striptx filepath
    case parse parse2Latex filepath filecontents of
        Left err ->
            putStrLn (parseErrorPretty err)
        Right latex -> do
            writeFile (fileRoot ++ ".tex") latex
            _ <- callProcess "latexmk"
                ["-pdf", "-interaction=nonstopmode", fileRoot ++ ".tex"]
            return ()
    removeLatexJunk fileRoot
@}

Trux just takes in one command-line argument, which is the name of the .tx file with the source code in it.  The first line gets this argument and saves it in filepath.  The fileRoot variable is the name of the file without its extension.  This is done using the striptx function:

@d funcStriptx @{
striptx :: String -> String
striptx filepath =
    case (reverse filepath) of
        'x' : 't' : '.' : htapelif -> reverse htapelif
        _ -> filepath
@}

All this does is take a string that ends in ``.tx" and returns the bit before the ".tx".

The case statement in the main function parses the file contents.  The function parse2Latex (see \ref{parse2Latex}) is the most important function in the whole program.  Its type signature is \texttt{parse2Latex :: Parser String} and it first parses the input file to an internal representation, and then converts this into Latex.   The \texttt{parse} function is part of Megaparsec, the parsing library used here.  It takes in a parser, a filepath (for making the error messages), and the contents of a file.  It returns an error, or the parsed contents.

If there is an error, it will be because the input file is wrong in some way.  Megarparsec was chosen as the parser for Trux because it has really good detailed error messages, which was one of the main motivations for building Trux in the first place.  The parseErrorPretty function is from Megaparsec again.  It just makes the error look nice and easy to read.

If there is no error, the Latex code that has been created is written to a .tex file.  This file is converted into a pdf using latexmk.  Latexmk is used because it handles running Latex the right number of times to create all the cross-references.  It also handles running the biber backend for generating citations from the references file.  (In Trux, the references file is an ordinary Bibtex file.  It must be called 'ref.bib').  The "-interaction=nonstopmode" argument to Latexmk is so that if there is an error then Latex just carries on running.  Really, unless some Latex package is not installed, there should be not errors in running Latex, because Trux should always generate correct Latex.  Any example where it does not is a bug.

The final line in the main function just removes the many files that Latex generates.  This is done because I find them annoying and don't read them anyway.  I think there is some performance loss on subsequent runs because of this, but I usually find that Latex is fast enough for this not to matter, and I do not like the clutter of all the files that Latex generates.  With Trux, you just run it and get a pdf and nothing else.

These junk files are removed with the removeLatexJunk function:

@d funcremoveLatexJunk @{
removeLatexJunk :: String -> IO()
removeLatexJunk fileRoot = mapM_ removeFileIfThere $ junkFiles fileRoot

junkFileExtensions :: [String]
junkFileExtensions =
    ["aux", "bbl", "bcf", "blg", "fdb_latexmk", "fls", "log", "out", "run.xml", "tex"]

junkFiles :: String -> [String]
junkFiles fileRoot = map ((fileRoot ++ ".") ++) junkFileExtensions

removeFileIfThere :: String -> IO()
removeFileIfThere filePath =
    removeFile filePath `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
@}

The import block for the Main module is:

@d mainImports @{
import System.Environment (getArgs)
import System.Process (callProcess)
import Parser (parse2Latex)
import Text.Megaparsec (parse, parseErrorPretty)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, throwIO)
@}

These are all external libraries, except for Parser, with its parse2Latex function.  This is the front of all the rest of the program.

So the whole Main module is like this:

@o nusrc/Main.hs @{
module Main where

@<mainImports@>
@<funcMain@>
@<funcStriptx@>
@<funcremoveLatexJunk@>
@}

\section{parse2Latex function} \label{parse2Latex}

This function is the most important part of the whole program:

@d funcParse2Latex @{
parse2Latex :: Parser String
parse2Latex = fmap doc2latex parseDocument
@}

The parsing library used in Trux is Megaparsec, chosen because it is pretty fast, has excellent error messages, and it quite easy to use.

The parseDocument function (see \ref{parseDocument}) is a Megaparsec parser with type signature \texttt{parseDocument :: Parser Document}.  ``Document" is the data type of the internal representation of the document.

The doc2latex function (see \ref{doc2latex}) takes a Document internal representation as its input, converts this to Latex and outputs a String.  Probably I should really have used Text instead of String, as it would be faster, but since it is currently plenty fast enough I will probably not bother to convert it.

\section{parseDocument function} \label{parseDocument}

This function is a Megaparsec parser.  Parsers are run by passing them to the Megarparsec parse function along with some content to parse.

@d funcParseDocument @{
parseDocument :: Parser Document
parseDocument = do
    header <- parseDocHeader
    body <- parseBody
    return $ Document header body
@}

So this function parses the input file contents into the internal Document data type.  The header of a document might be a title and the date and the author, and the body is the main content, like paragraphs of text, tables, images etc.  See \cref{parseDocHeader} for the parseDocHeader function and \cref{parseBody} for the parseBody function.

The Document data type is:
@d Document @{
data Document = Document (Maybe DocHeader) DocumentBody deriving Show
@}

So the header is optional, but all documents must have a body.

\section{parseDocHeader function} \label{parseDocHeader}

\section{parseBody function} \label{parseBody}

This function parses the header of the document if there is one.  The header is optional, and contains things such as a title, author and date.

@d funcParseDocHeader @{
parseDocHeader :: Parser (Maybe DocHeader)
parseDocHeader =
    (do
        title <- parseTitle
        author <- parseAuthor
        date <- parseDate
        return $ Just $ DocHeader title author date) <|>
    return Nothing
@}

The $<|>$ function is part of Megaparsec.  Roughly what it means is ``try to run the parser on the left, but if it fails, run the parser on the right.".  So it will try to parse the title, the author and the date, but if that does not work it will assume that there is no header and return Nothing.

The DocHeader data type is:

@d dataDocHeader @{
data DocHeader = DocHeader Title (Maybe Author) (Maybe Date) deriving Show
@}

So a document header must contain a title, but the author and date are optional.

The parseTitle function is:

@d funcParseTitle @{
parseTitle :: Parser Title
parseTitle = do
    _ <- parseFuncName "title"
    title <- parseList bracket1 bracket2 parseHeaderElement
    return $ Title title
@}

The Title data type is:

@d dataTitle @{
newtype Title = Title [Element] deriving Show
@}

So a title is a list of of Elements (see \cref{dataElement}).

\section{Element data type} \label{dataElement}
An Element is the main building block out of which a document is made:

@d dataElement @{
data Element
  = Text String
  | MathElement Math
  | Header Numbered HeaderLevel [Element]
  | CrossReference Id
  | Citation Id
  | Umlaut Char
  | ElementSimpleSub String
  | Table Id [Element] [[[Element]]]
  | Image Id Float [Element] String
    deriving Show
@}

\begin{itemize}
\item Text is ordinary text, like a paragraph.
\item MathElement is an equation or other piece of mathematical notation.  It can be either inline or on its own block.
\item Header is the header of a section.  Numbered contains instructions about how to number and reference the section (see \cref{dataNumbered}).  There are three types of HeaderLevel, for sections, subsections and subsubsections:
@d dataHeaderLevel @{
data HeaderLevel = HeaderOne | HeaderTwo | HeaderThree deriving Show
@}
A header contains a restricted list of Elements, that is, some elements, like images or tables, are not allowed in a header.
\item A CrossReference only has an Id (see \cref{dataNumbered}), which refers to another section.  A CrossReference is an internal link to another part of the document.
\item A Citation refers to something in the bibliography file.  This is an ordinary Bibtex file called ref.bib.
\item An Umlaut is for putting umlauts over a character.
\item ElementSimpleSub is for things in Trux that map very simply to things in Latex, like symbols.
\item A Table has an Id so it can be linked to with CrossReferences, a title, which is a restricted list of Elements, and a list of lists of lists of Elements, which are the cells (a cell is a list of elements), the rows (a list of cells) and the whole table (a list of rows).
\item An Image is used for inserting images into a document.  So it has an Id to allow making CrossReferences to it, a Float which is the width to display it as, a restricted list of Elements for the caption, and a String, which is the name of the file the image is stored in.
\end{itemize}

\section{Numbered data type} \label{dataNumbered}

This data type is used for all the elements that can have a number by them, and can therefore have a label that can be referenced from elsewhere.  It is:

@d dataNumbered @{
data Numbered = NumberOn Id | NumberOff deriving Show
@}

Id is just a newtype wrapper for String:

@d dataId @{
newtype Id = Id String deriving Show
@}

So an element that has Numbered in it can either be numbered or not, and if it is numbered, it also has a unique ID and can be referenced from elsewhere.  That is actually one current bug in Trux, that it does not check that all the IDs are unique.  I may not ever bother to fix this as it is not too serious.

\section{doc2latex function} \label{doc2latex}

The doc2latex function converts the internal Document representation into a valid Latex string.  It does not have to worry about user errors, since these should all have been caught by the parser that converts the user input file to the internal Document representation.

@d funcDoc2latex @{
doc2latex :: Document -> String
doc2latex (Document header body) =
    unlines [preamble, docHeader2latex header, body2latex header body]
@}

The preamble is a string which is put at the top of all files, and contains mostly latex package imports (see \cref{preamble}).

The docHeader to Latex function (see \cref{docHeader2latex}) converts the internal representation of the document header into a Latex string.  The body2latex function (see \cref{body2latex}) does a similar thing for the body.

\section{docHeader2latex function} \label{docHeader2latex}

It converts the internal representation of a document header (see \cref{parseBody} into a Latex string.

@d funcDocHeader2latex @{
docHeader2latex :: Maybe DocHeader -> String
docHeader2latex Nothing = ""
docHeader2latex (Just (DocHeader title author date)) =
    unlines [title2latex title, author2latex author, date2latex date]
@}

The reason the input is a Maybe DocHeader is that the header is optional.  Each of the functions in the list outputs a string, and the unlines function joins the strings together with a newline between each one.

\section{body2latex function} \label{body2latex}

\section{Latex preamble} \label{preamble}

This is the string that is put at the top of the .tex Latex file that is produced by trux, before it is converted into a pdf with Latex.

@d funcPreamble @{
preamble :: String
preamble =
    "\\documentclass{article}\n\
    \\\usepackage[utf8]{inputenc}\n\
    \\\usepackage{microtype}\n\
    \\\usepackage[hidelinks]{hyperref}\n\
    \\\usepackage{amsmath}\n\
    \\\usepackage{amsfonts}\n\
    \\\usepackage[style=authoryear]{biblatex}\n\
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
    \\\crefdefaultlabelformat{#2#1#3}\n"
@}

Briefly, it does the following:

\begin{itemize}
\item article:  The traditional Latex article style.  I still prefer the look of this over scrartcl.  Perhaps because it is what I am used to.  I like the wide margins.
\item inputenc: sets the file encoding (I think)
\item microtype: makes the whole document look subtly nicer, with better line breaking among other things
\item hyperref: makes the internal links clickable
\item amsmath and amsfonts: adds extra mathematical functionality
\item biblatex: for citations
\item bibliography: gives the name of the bibliography file
\item graphicx: for inserting images
\item bm: for bold characters in math mode
\item float: for placing tables and images (see \cref{floatPlacement})
\item textcomp: provides extra symbols
\item commath: for better derivatives, like upright d in dx and nice curly ds for partial derivatives
\item siunitx: for nice neat unit abbreviations, like metres and seconds
\item eurosym: for proper euro symbols
\item frenchspacing: This makes single spaces after sentences.  This was chosen because otherwise the user has to worry about making single spaces after spaces for abbreviations.  For example, Latex things that writing Mr. Smith means that the sentence ends after the Mr., and puts a double space in there.
\item booktabs: This is used for better tables.  It gives subtle things that make them look beautiful, like slightly bigger spacing around horizontal lines, and slightly thicker lines at the bottom and top.
\item breqn: This is used for automatically aligning equations neatly.
\item cleveref: It cleverly puts in the right label in front of internal links, like `section' in front of a section number or `table' in front of a table number.
\item cref...: some settings for cleverref - I can't quite remember what they do off-hand but I think they are important.
\end{itemize}

\section{Placement of tables and images} \label{floatPlacement}

Trux places tables and images exactly where they appear in the source file, so it ignores all Latex's cleverness in putting them where they fit in better with the flow in the text.  This is probably quite a controversial decision, as I have often been told that `Latex knows best' about where to place things, and for many years I have followed this advice and left my tables and images to go wherever Latex puts them.

However I have finally come to the conclusion that:
\begin{enumerate}
\item New users to Latex find this behaviour annoying and peculiar.
\item I still find this behaviour more annoying than it is useful.
\item Latex often puts things where I think they should not be.
\item I think there is a real advantage to things being right next to where they were referred to.  It makes the document more readable.
\end{enumerate}

And I think there is still a benefit to using a float rather than just inserting the item, because it handles the vertical spacing and the caption so that it looks nice.
\printbibliography
\end{document}

