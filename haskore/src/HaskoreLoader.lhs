\section{The Architecture of Haskore}

Figure \ref{haskore} shows the overall structure of Haskore.  Note the
independence of high level structures from the ``music platform'' on
which Haskore runs.  Originally, the goal was for Haskore compositions
to run equally well as conventional midi-files \cite{midi}, NeXT
MusicKit score files \cite{musickit}, and csound score files
\cite{csound}, and for Haskore compositions to be displayed and
printed in traditional notation using the CMN (Common Music Notation)
subsystem.  In reality, only one of these platforms is currently
supported: midi.  It is probably the most popular platform for users,
and its use with Haskore is described in detail in this
tutorial.\footnote{(1) The NeXT music platform is obsolete.  (2) There
exists a translation to csound for an earlier version of Haskore that
I would happily share with anyone interested.  (3) We have abandoned
CMN entirely, as there are now better candidates for notation packages
into which Haskore could be mapped.}

\begin{figure*}
\centerline{
\epsfysize=4.0in 
\epsfbox{Pics/haskore.eps}
}
\caption{Overall System Diagram}
\label{haskore}
\end{figure*}

In any case, the independence of abstract musical ideas from the
concrete rendering platform is accomplished by having abstract notions
of {\em musical object}, {\em player}, {\em instrument}, and {\em
performance}.  All of this resides in the box labeled ``Haskore'' in
the diagram above.

At the module level, Haskore is organized as follows:
\begin{verbatim} 

> module HaskoreLoader (module HaskoreLoader, 
>                       -- module Players,
>                       module HaskToMidi, module TestHaskore, module ReadMidi)
>        where
>
> import Basics         -- described in Section 3
> import Performance    -- described in Section 4
> -- import Players     -- described in Section 5
> import HaskToMidi     -- described in Section 6
> import TestHaskore
> import ReadMidi

\end{verbatim} 

This document was written in the {\em literate programming style}, and
thus the \LaTeX\ manuscript file from which it was generated is an
{\em executable Haskell program}.  It can be compiled under \LaTeX\ in
two ways: a basic mode provides all of the functionality that most
users will need, and an extended mode in which various pieces of
lower-level code are provided and documented as well (see file header
for details).  This version was compiled in
\basic{basic}\extended{extended} mode.  The document can be retrieved
via WWW from: {\tt http://haskell.org/haskore} (consult the README
file for details).  It is also delivered with the standard joint
Nottingham/Yale Hugs release.

The Haskore code conforms to Haskell 1.4, and has been tested under
the June, 1998 release of Hugs 1.4.  Unfortunately Hugs does not yet
support mutually recursive modules, so all references to the module
{\tt Players} in this document are commented out, which in effect
makes it part of module {\tt Performance} (with which it is mutually
recursive).

A final word before beginning: As various musical ideas are presented
in this Haskore tutorial, I urge the reader to question the design
decisions that are made.  There is no supreme theory of music that
dictates my decisions, and what I present is actually one of several
versions that I have developed over the years (this version is much
richer than the one described in \cite{haskore}; it is the ``Haskore
in practice'' version alluded to in Section 6 of that paper).  I
believe that this version is suitable for many practical purposes, but
the reader may wish to modify it to better satisfy her intuitions
and/or application.
