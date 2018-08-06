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
    let fileRoot = (striptx filepath)
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

Trux just takes in one command-line argument, which is the name of the .tx file with the source code in it.  The first line gets this argument and saves it in filepath.  The fileRoot variable is the name of the file without its extension.

The case statement parses the file contents.  The function parse2Latex (see \ref{parse2Latex}) is the most important function in the whole program.  Its type signature is \texttt{parse2Latex :: Parser String} and it first parses the input file to an internal representation, and then converts this into Latex.   The \texttt{parse} function is part of Megaparsec, the parsing library used here.  It takes in a parser, a filepath (for making the error messages), and the contents of a file.  It returns an error, or the parsed contents.

If there is an error, it will be because the input file is wrong in some way.  Megarparsec was chosen as the parser for Trux because it has really good detailed error messages, which was one of the main motivations for building Trux in the first place.  The parseErrorPretty function is from Megaparsec again.  It just makes the error look nice and easy to read.

If there is no error, the Latex code that has been created is written to a .tex file.  This file is converted into a pdf using latexmk.  Latexmk is used because it handles running Latex the right number of times to create all the cross-references.  It also handles running the biber backend for generating citations from the references file.  (In Trux, the references file is an ordinary Bibtex file.  It must be called 'ref.bib').  The "-interaction=nonstopmode" argument to Latexmk is so that if there is an error then Latex just carries on running.  Really, unless some Latex package is not installed, there should be not errors in running Latex, because Trux should always generate correct Latex.  Any example where it does not is a bug.

The final line in the main function just removes the many files that Latex generates.  This is done because I find them annoying and don't read them anyway.  I think there is some performance loss on subsequent runs because of this, but I usually find that Latex is fast enough for this not to matter, and I do not like the clutter of all the files that Latex generates.  With Trux, you just run it and get a pdf and nothing else.

\section{parse2Latex function} \label{parse2Latex}

\printbibliography
\end{document}

