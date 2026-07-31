%%
%% thesis.cls — framework default thesis class for dissertation.ai-provisioned dissertations.
%%
%% This is the "shipped" mode default per the 3-mode strategy documented in
%% `THESIS-CLS-README.md`. If your institution provides an official .cls or
%% publishes formatting guidelines, replace or refine this file accordingly.
%%
%% Built on the `book` document class with thesis-appropriate defaults +
%% macros for title page, signature page, and front-/main-/back-matter
%% transitions.
%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{thesis}[2026/01/01 v0.1.0 dissertation.ai framework default thesis class]

% Pass all options through to the book class.
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}
\ProcessOptions\relax
\LoadClass[oneside,12pt]{book}

% Standard packages.
\RequirePackage{geometry}
\RequirePackage{setspace}
\RequirePackage{etoolbox}
\RequirePackage{xstring}

% Margins — 1in default. Many institutions require 1.5in left for binding;
% Set margins here only; do NOT also set `geometry:` in _quarto.yml — that
% reloads the package and triggers "Option clash for package geometry".
\geometry{margin=1in}

% Double-spacing by default; many institutions require this.
\onehalfspacing

% Title page macro.
% Usage: \thesistitle{Title}{Author}{Institution}{Degree program}{Year}
\newcommand{\thesistitle}[5]{%
  \begin{titlepage}
    \centering
    \vspace*{2in}
    {\Huge\bfseries #1\par}
    \vspace{1.5in}
    {\Large by #2\par}
    \vspace{2in}
    {\large A dissertation submitted to #3\par}
    \vspace{0.25in}
    {\large in partial fulfillment of the requirements\par}
    \vspace{0.25in}
    {\large for the degree of #4\par}
    \vfill
    {\large #5\par}
  \end{titlepage}
}

% Signature page macro.
% Usage: \signaturepage{Chair name}{Committee member 1}{Committee member 2}{...}
\newcommand{\signaturepage}[1]{%
  \clearpage
  \thispagestyle{empty}
  \begin{center}
    \large\bfseries Approved by the Dissertation Committee
  \end{center}
  \vspace{1in}
  #1
  \clearpage
}

% Helper: signature line.
\newcommand{\sigline}[1]{%
  \noindent\rule{4in}{0.4pt}\\
  #1 \\[1.5em]
}

% Abstract environment — committee-required preceding metadata block.
\newenvironment{abstract}{%
  \clearpage
  \thispagestyle{empty}
  \begin{center}
    \large\bfseries Abstract
  \end{center}
  \vspace{0.5in}
}{%
  \clearpage
}

% Front-/main-/back-matter transitions inherit from book; \frontmatter,
% \mainmatter, \backmatter are available unchanged. Page numbering: Roman
% lowercase in frontmatter, Arabic in mainmatter, continuing Arabic in
% backmatter.

\endinput
