\documentclass{article}
\usepackage{listings}
\usepackage[hidelinks]{hyperref}
\usepackage{microtype}
\title{Trux}
\author{True Ghiassi}

% Run this with 
% nuweb -r min.w

\begin{document}
\maketitle
Trux has two elements:
\begin{itemize}
\item a plain-text document markup language (see Section \ref{markupLanguage}
\item a program to convert the markup language into documents that are easy to read (see Section \ref{compiler})
\end{itemize}
The aims of Trux are:
\begin{enumerate}
\item to produce documents that are of high quality
\item to be easy to use
\end{enumerate}
Aim 1 is more important than 2.

Some existing alternatives to Trux were considered before this project was begun (see Section \ref{alternatives}).

This document is written using Nuweb \cite{nuweb}, where the code and documentation are combined in one document.  The code snippets in the text are in Haskell \cite{Haskell}.
\section{Alternatives to Trux} \label{alternatives}
Some other alternatives I used or looked into before beginning this project were:
\begin{itemize}
\item Latex: The output is beautiful but there are so many options that it is very slow and difficult for a newcomer to do a simple task, such as for an undergraduate engineering student to type a neat lab report.
\item Pandoc and markdown: easy to use, but uses Latex notation for mathematics, see objection above.
\item Microsoft Word: Convenient to use, but the output is not quite so nice as Latex.  I also miss the simplicity of a plain-text input file, because I like to see all the formatting, whereas Word hides the document structure away.
\end{itemize}
\section{Document markup language} \label{markupLanguage}
A Trux document is made of a list of elements (see Section \ref{element}), separated by single spaces or single newlines.
\section{Element} \label{element}
An element is either an atom (see Section \ref{atom}), a function (see Section \ref{function}), or a string (see Section \ref{string}).
\section{Atom} \label{atom}
An atom is a sequence of one or more of these characters:
@d atomChars @{
atomChars :: String
atomChars =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \0123456789\
    \¬!\"$%^&*()_-+=[];:'#~.,<>?/"
@}
The beginning of an atom is marked by one of:
\begin{itemize}
\item the beginning of input
\item a space
\item a newline
\item an opening curly bracket `\{'
\end{itemize}
The end of an atom is marked by one of:
\begin{itemize}
\item the end of input
\item a space
\item a newline
\item a closing curly bracket `\}'
\end{itemize}
\section{Function}\label{function}
A function has a name and a list of arguments.  Trux does not allow the definition of new functions, only the use of built-in ones.  A function is called by writing its name, which is a sequence of characters as defined in Section \ref{atom}, followed by an opening curly bracket `\{', followed by a list of elements (see Section \ref{element}), followed by a closing curly bracket `\}'.  The elements in the function are separated by single spaces or single newlines.
\section{String} \label{string}
A string is a sequence of the characters defined by:
@d stringChars @{
stringChars :: String
stringChars =
    "abcdefghijklmnopqrstuvwxyz\
    \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
    \!\"£$%^&*()_-=+{}[];:\'#~,<.>/?\\|"
@}
The beginning and end of a string are marked by single backticks.
\section{
\end{document}

