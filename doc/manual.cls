%% LaTeX2e package for technical reports.
%%
%% Copyright (C) 2001, 2002 Thomas Mailund
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


%% --- IDENTIFICATIN -----------------------------------------------
\NeedsTeXFormat{LaTeX2e}%
\ProvidesClass{techreport}[2002/07/01]%

%% --- INITIAL CODE ------------------------------------------------

%% --- DECLARATION OF OPTIONS --------------------------------------
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

%% --- EXECUTION OF OPTIONS ----------------------------------------
\ProcessOptions%

%% --- PACKAGE LOADING ---------------------------------------------
\LoadClass[11pt,twoside,a4paper,openright]{article}
\RequirePackage{xspace}%
\RequirePackage[nooneline,bf]{caption}%
\RequirePackage{amsmath,amssymb}%
\RequirePackage{fancyhdr}
\RequirePackage{a4}
\RequirePackage{makeidx}
\RequirePackage[english]{babel}%
\RequirePackage{graphicx}

\RequirePackage{subfigure}
\RequirePackage{boxedminipage}
\RequirePackage[dvips,all]{xy}
\RequirePackage[sort]{cite}

%\RequirePackage{palatino}
\usepackage{charter}            %for body
\usepackage{helvet}             %for sf
\usepackage{euler}              %for math

\bibliographystyle{amsplain}

\usepackage{alltt}
\newenvironment{code}
  {\vspace*{-.1\baselineskip}\begin{alltt}\small}
  {\normalsize\end{alltt}\vspace*{-.2\baselineskip}}

%% --- MAIN CODE ---------------------------------------------------

%% Title of the report
\def\thetitle{Report Title}
\renewcommand{\title}[1]{\def\thetitle{#1}}

%% Subtitle of the report
\def\thesubtitle{}
\newcommand{\subtitle}[1]{\def\thesubtitle{#1}}

%% Series and number
\def\thereportseries{STATES Report Series}
\newcommand{\reportseries}[1]{\def\thereportseries{#1}}
\def\thereportno{SRS-?}
\newcommand{\reportno}[1]{\def\thereportno{#1}}

%% Authors
\def\theauthors{}
\newcommand{\authors}[1]{\def\theauthors{#1}}
\def\thecontact{}
\newcommand{\contact}[1]{\def\thecontact{#1}}
\def\thecompany{Bioinformatics ApS}
\newcommand{\company}[1]{\def\thecompany{#1}}
\def\thetoolversion{SomeTool v.0.0}
\newcommand{\toolversion}[1]{\def\thetoolversion{#1}}

%% the year
\def\theyear{\number\year}
\newcommand{\copyrightyear}[1]{\def\theyear{#1}}
\def\themonth{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi}
\newcommand{\copyrightmonth}[1]{\def\themonth{#1}}

%% Abstract
\def\theabstract{}
\renewcommand{\abstract}[1]{\def\theabstract{#1}}


\AtBeginDocument{
  %% COVER TITLE PAGE
  \pagenumbering{roman}
  \begin{titlepage}
    \par
    \thispagestyle{empty}
    \mbox{}\vspace{0.1\textheight}
    \hrule
    \vspace{0.05\textheight}
    \raggedleft
    \textbf{\huge \thetitle}\\[18pt]
    \textsf{\large \thesubtitle}\\
    \vspace{0.05\textheight}
    \hrule
    \vfill
    {\Large\theauthors}\\
    \vspace{0.01\textheight}
    {\large\thecontact}\\
    \vfill
    {\small\textsf{\thecompany\hfill\thetoolversion}}\\
    \vspace{.3 em}
    \hrule
    \vspace{0.1 em}
    {\small\textsf{\hfill\themonth\ \theyear}}
  \end{titlepage}
  \clearpage

  %% BACKSIDE OF COVER, for (C) and stuff.
   \thispagestyle{empty}
   \mbox{}\vfill\noindent
   \textsf{Copyright \copyright\ \theyear\ \theauthors\ $\bullet$ \thecompany}
   \vspace{0.5\baselineskip}\\
   \textsf{Permission is granted to make and distribute
   verbatim copies of this manual provided the copyright notice and
   this permission notice are preserved in all copies.}
   \clearpage


   % AND *FINALLY* MAKING READY FOR THE REAL DOCUMENT...
   \cleardoublepage
   \pagenumbering{arabic}
 }


%% Page Layout
%\pagestyle{myheadings}
%\renewcommand{\chaptermark}[1]{\markboth{\textit{\chaptername\ \thechapter. #1}}{}} 
%\renewcommand{\sectionmark}[1]{\markright{\textit{\thesection. #1}}}

%% captions
\addtolength{\captionmargin}{3.5mm}
\setlength\abovecaptionskip{1.5pt}
\setlength\belowcaptionskip{.5pt}

%% paragraphs
\parindent 2 ex
\addtolength{\parskip}{2 pt}

%% \def\@makechapterhead#1{%
%%   \vspace*{30\p@}%
%%   {\parindent \z@ \raggedright \normalfont
%%     \ifnum \c@secnumdepth >\m@ne
%%        \Large\textsc \@chapapp\space \thechapter
%%        \par\nobreak
%%     \fi
%%     \interlinepenalty\@M
%%     \Huge\bfseries #1\par\nobreak
%%     \par\vskip 30\p@
%%   }}

\setcounter{secnumdepth}{0} % no numbering
\renewcommand\section{\@startsection{section}{1}
                       {\z@}%
                       %{-30pt}%
                       {-1\baselineskip}%
                       {0.2\baselineskip}%
                       {\normalfont\sffamily\Large\bfseries\boldmath
                        }}%\rightskip=\z@ \@plus 8em\pretolerance=10000 }}
\renewcommand\subsection{\@startsection{subsection}{2}%
                       {\z@}%
                       %{-30pt}%
                       {-1\baselineskip}%
                       {0.2\baselineskip}%
                       {\normalfont\sffamily\large\itshape%\bfseries\boldmath
                        }}%\rightskip=\z@ \@plus 8em\pretolerance=10000 }}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                       {-18\p@ \@plus -4\p@ \@minus -4\p@}%
                       {-0.5em \@plus -0.22em \@minus -0.1em}%
                       {\normalfont\sffamily\normalsize\bfseries\boldmath}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                       {-12\p@ \@plus -4\p@ \@minus -4\p@}%
                       {-0.5em \@plus -0.22em \@minus -0.1em}%
                       {\normalfont\sffamily\normalsize\itshape}}
