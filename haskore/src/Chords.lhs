\section{Representing Chords}
\label{chords}

Earlier I described how to represent chords as values of type {\tt
Music}.  However, sometimes it is convenient to treat chords more
abstractly.  Rather than think of a chord in terms of its actual
notes, it is useful to think of it in terms of its chord ``quality,''
coupled with the key it is played in and the particular voicing used.
For example, we can describe a chord as being a ``major triad in root
position, with root middle C.''  Several approaches have been put
forth for representing this information, and we cannot cover all of
them here.  Rather, I will describe two basic representations, leaving
other alternatives to the skill and imagination of the
reader.\footnote{For example, Forte prescribes normal forms for chords
in an atonal setting \cite{forte}.}

First, one could use a {\em pitch} representation, where each note is
represented as its distance from some fixed pitch.  {\tt 0} is the
obvious fixed pitch to use, and thus, for example, {\tt [0,4,7]}
represents a major triad in root position.  The first zero is in some
sense redundant, of course, but it serves to remind us that the chord
is in ``normal form.''  For example, when forming and transforming
chords, we may end up with a representation such as {\tt [2,6,9]},
which is not normalized; its normal form is in fact {\tt [0,4,7]}.
Thus we define:
\begin{quote}
A chord is in {\em pitch normal form} if the first pitch is zero,
and the subsequent pitches are monotonically increasing.
\end{quote}

One could also represent a chord {\em intervalically}; i.e.~as a
sequence of intervals.  A major triad in root position, for example,
would be represented as {\tt [4,3,-7]}, where the last interval
``returns'' us to the ``origin.'' Like the {\tt 0} in the pitch
representation, the last interval is redundant, but allows us to
define another sense of normal form:
\begin{quote}
A chord is in {\em interval normal form} if the intervals are all
greater than zero, except for the last which must be equal to the
negation of the sum of the others.
\end{quote}
In either case, we can define a chord type as:
\begin{verbatim} 

> type Chord = [AbsPitch]

\end{verbatim} 

We might ask whether there is some advantage, computationally, of
using one of these representations over the other.  However, there is
an invertible linear transformation between them, as defined by the
following functions, and thus there is in fact little advantage of one
over the other:
\begin{verbatim}

> pitToInt :: Chord -> Chord
> pitToInt ch = aux ch
>    where aux (n1:n2:ns) = (n2-n1) : aux (n2:ns)
>          aux [n]        = [head ch - n]
>
> intToPit :: Chord -> Chord
> intToPit ch = 0 : aux 0 ch
>    where aux p [n]    = []
>          aux p (n:ns) = n' : aux n' ns  where n' = p+n

\end{verbatim} 

\begin{exercise}
Show that {\tt pitToInt} and {\tt intToPit} are {\em inverses} in the
following sense: for any chord {\tt ch1} in pitch normal form, and
{\tt ch2} in interval normal form, each of length at least two:
\begin{center}
{\tt intToPit (pitToInt ch1) = ch1}\\
{\tt pitToInt (intToPit ch2) = ch2}
\end{center}
\end{exercise}

Another operation we may wish to perform is a test for {\em equality}
on chords, which can be done at many levels: based only on chord
quality, taking inversion into account, absolute equality, etc.  Since
the above normal forms guarantee a unique representation, equality of
chords with respect to chord quality and inversion is simple: it is
just the standard (overloaded) equality operator on lists.  On the
other hand, to measure equality based on chord quality alone, we need
to account for the notion of an {\em inversion}.

Using the pitch representation, the inversion of a chord can be
defined as follows:
\begin{verbatim} 

> pitInvert (p1:p2:ps) = 0 : map (subtract p2) ps ++ [12-p2]

\end{verbatim} 
Although we could also directly define a function to invert
a chord given in interval representation, we will simply
define it in terms of functions already defined:
\begin{verbatim} 

> intInvert = pitToInt . pitInvert . intToPit

\end{verbatim}
% pitInvert [0,4,7] => [4,7,0] => [0,3,-4] => [0,3,8]
% intInvert [4,3,-7] => [3,-7,4] => [3,5,4] => [3,5,-8]

We can now determine whether a chord in normal form has the same
quality (but possibly different inversion) as another chord in normal
form, as follows: simply test whether one chord is equal either to the
other chord or to one of its inversions.  Since there is only a finite
number of inversions, this is well defined.  In Haskell:
\begin{verbatim} 

> samePitChord ch1 ch2 = 
>  let invs = take (length ch1) (iterate pitInvert ch1)
>  in  ch2 `elem` invs
>
> sameIntChord ch1 ch2 = 
>  let invs = take (length ch1) (iterate intInvert ch1)
>  in  ch2 `elem` invs

\end{verbatim} 
For example, {\tt samePitChord [0,4,7] [0,5,9]} returns {\tt True}
(since {\tt [0,5,9]} is the pitch normal form for the second inversion
of {\tt [0,4,7]}).

