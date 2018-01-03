\documentclass{article}
\usepackage{listings}
\usepackage[hidelinks]{hyperref}
\usepackage{microtype}
\usepackage{booktabs}
\title{Trux}
\author{True Ghiassi}

\begin{document}
\maketitle
Trux has two elements:
\begin{itemize}
\item a plain-text document markup language (see Section \ref{markupLanguage})
\item a program to convert the markup language into documents that are easy to read (see Section \ref{compiler})
\end{itemize}
The aims of Trux are:
\begin{enumerate}
\item to produce documents that are of high quality
\item to be easy to use
\end{enumerate}
Aim 1 is more important than 2.

This document is written using Nuweb \cite{nuweb}, where the code and documentation are combined in one document.  The code snippets in the text are in Haskell \cite{Haskell}.
\section{Document markup language} \label{markupLanguage}
A Trux document is a plain text file containing a list of elements (see Section \ref{element}), separated by single spaces or single newlines.
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
A function has a name and a list of arguments.  Trux does not allow the definition of new functions, only the use of built-in ones (see Table \ref{built-in-functions}).  A function is called by writing its name, which is a sequence of characters as defined in Section \ref{atom}, followed by an opening curly bracket `\{', followed by a list of elements (see Section \ref{element}), followed by a closing curly bracket `\}'.  The elements in the function are separated by single spaces or single newlines.

\begin{table}
\begin{tabular}{ll}\toprule
Name & Section number \\ \midrule
m & \ref{inlineMath} \\
M & \ref{displayMath}
\\ \bottomrule
\end{tabular}
\caption{Built-in function names and corresponding section numbers}
\label{built-in-functions}
\end{table}

\section{Function `m' for inline mathematics} \label{inlineMath}
It is used for making mathematical notation that is inline with ordinary text, such this equation: $y = 3x^2$.  The arguments for the function are provided in the same way as for the `M' function for separately displayed mathematics (see Section \ref{displayMath}).
\section{Function `M' for separately displayed mathematics} \label{displayMath}
It is for making mathematical notation that is displayed separately on its own line, such as this equation:
\[y=3x^2\]
The built-in functions that can be called inside this function are shown in Table \ref{mathFunctions}.  The built-in atoms that can be used inside this function are shown in Table \ref{mathAtoms}.
\begin{table}
\begin{tabular}{ll} \toprule
Name & Section number \\ \midrule
\textasciicircum & \ref{mathPowerFunction} \\
\_ & \ref{mathSubscriptFunction}
\\ \bottomrule
\end{tabular}
\caption{Functions that can be used inside the mathematics functions}
\label{mathFunctions}
\end{table}
\section{Function `\textasciicircum' for mathematical superscripts} \label{mathPowerFunction}
This function takes 
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

\end{document}

