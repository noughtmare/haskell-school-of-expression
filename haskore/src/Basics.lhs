\section{The Basics}
\label{basics}

\begin{verbatim} 

> module Basics where
> import Data.Ix
> import Data.Ratio
> infixr 5 :+:, :=:

\end{verbatim} 
Perhaps the most basic musical idea is that of a {\em pitch}, which
consists of a {\em pitch class} (i.e. one of 12 semi-tones) and an
{\em octave}:
\begin{verbatim} 

> type Pitch      = (PitchClass, Octave)
> data PitchClass = Cf | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F | Fs
>                 | Gf | G | Gs | Af | A | As | Bf | B | Bs
>      deriving (Eq,Ord,Ix,Show,Read)
> type Octave     = Int

\end{verbatim} 
So a {\tt Pitch} is a pair consisting of a pitch class and an octave.
Octaves are just integers, but we define a datatype for pitch classes,
since distinguishing enharmonics (such as G\# and Ab) may be important
(especially for notation).  By convention, A440 = {\tt (A,4)}.

Musical objects are captured by the {\tt Music} datatype:\footnote{I
prefer to call these ``musical objects'' rather than ``musical
values'' because the latter may be confused with musical aesthetics.}
\begin{verbatim} 

> data Music = Note Pitch Dur [NoteAttribute]   -- a note \ atomic 
>            | Rest Dur                         -- a rest /    objects
>            | Music :+: Music                  -- sequential composition
>            | Music :=: Music                  -- parallel composition
>            | Tempo  (Ratio Int) Music         -- scale the tempo
>            | Trans  Int Music                 -- transposition
>            | Instr  IName Music               -- instrument label
>            | Player PName Music               -- player label
>            | Phrase [PhraseAttribute] Music   -- phrase attributes
>     deriving (Show, Eq)
>
> type Dur   = Ratio Int                        -- in whole notes
> type IName = String
> type PName = String

\end{verbatim} 
Here a {\tt Note} is its pitch paired with its duration (in number of
whole notes), along with a list of {\tt NoteAttributes} (defined
later).  A {\tt Rest} also has a duration, but of course no pitch or
other attributes.

Note that durations are represented as rational numbers; specifically,
as ratios of two Haskore {\tt Int} values.  Previous versions of
Haskore used floating-point numbers, but rational numbers are more
precise (as long as the {\tt Int} values do not exceed the maximum
allowable).

From these two atomic constructors we can build more complex musical
objects using the other constructors, as follows:
\begin{itemize}
\item {\tt m1 :+: m2} is the sequential composition of {\tt m1} and
{\tt m2}; i.e.\ {\tt m1} and {\tt m2} are played in sequence.
\item {\tt m1 :=: m2} is the parallel   composition of {\tt m1} and
{\tt m2}; i.e.\ {\tt m1} and {\tt m2} are played simultaneously.
\item {\tt Tempo a m} scales the rate at which
{\tt m} is played (i.e.\ its tempo) by a factor of {\tt a}.
\item {\tt Trans i m} transposes {\tt m} by interval {\tt i} (in semitones).
\item {\tt Instr iname m} declares that {\tt m} is to be performed using
instrument {\tt iname}.
\item {\tt Player pname m} declares that {\tt m} is to be performed by
player {\tt pname}.
\item {\tt Phrase pas m} declares that {\tt m} is to be played using
the phrase attributes (described later) in the list {\tt pas}.
\end{itemize}

It is convenient to represent these ideas in Haskell as a recursive
datatype because we wish to not only construct musical objects, but
also take them apart, analyze their structure, print them in a
structure-preserving way, interpret them for performance purposes,
etc.

\subsection{Convenient Auxiliary Functions}
\label{auxiliaries}

In anticipation of the need to translate between different number
types, we define the following coercion functions:
\begin{verbatim} 

> rtof  :: Ratio Int -> Float
> rtof r = float (numerator r) / float (denominator r)
>
> float :: Int -> Float
> float  = fromInteger . toInteger

\end{verbatim} 
Treating pitches simply as integers is also useful in many settings,
so let's also define some functions for converting between {\tt Pitch}
values and {\tt AbsPitch} values (integers):
\begin{verbatim} 

> type AbsPitch = Int
>
> absPitch :: Pitch -> AbsPitch
> absPitch (pc,oct) = 12*oct + pitchClass pc
>
> pitch    :: AbsPitch -> Pitch
> pitch    ap       = ( [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! mod ap 12, 
>                       quot ap 12)
>
> pitchClass :: PitchClass -> Int
> pitchClass pc = case pc of
>      Cf -> -1;  C -> 0;  Cs -> 1    -- or should Cf be 11?
>      Df -> 1;   D -> 2;  Ds -> 3
>      Ef -> 3;   E -> 4;  Es -> 5
>      Ff -> 4;   F -> 5;  Fs -> 6
>      Gf -> 6;   G -> 7;  Gs -> 8
>      Af -> 8;   A -> 9;  As -> 10
>      Bf -> 10;  B -> 11; Bs -> 12   -- or should Bs be 0?

\end{verbatim} 
We can also define a function {\tt trans}, which transposes pitches
(analogous to {\tt Trans}, which transposes values of type {\tt
Music}):
\begin{verbatim} 

> trans    :: Int -> Pitch -> Pitch
> trans i p = pitch (absPitch p + i)

\end{verbatim} 
Finally, for convenience, let's create simple names for familiar
notes, durations, and rests, as shown in Figure \ref{note-names}.
Despite the large number of them, these names are sufficiently
``unusual'' that name clashes are unlikely.

\begin{exercise} Show that\ \ {\tt abspitch\ .\ pitch = id}, and,
up to enharmonic equivalences,\newline {\tt pitch\ .\ abspitch = id}.
\end{exercise}

\begin{exercise} Show that\ \ {\tt trans i (trans j p) = trans (i+j) p}.
\end{exercise}

\begin{figure}{\small
\begin{verbatim} 

> cf,c,cs,df,d,ds,ef,e,es,ff,f,fs,gf,g,gs,af,a,as,bf,b,bs :: 
>    Octave -> Dur -> [NoteAttribute] -> Music
>
> cf o = Note (Cf,o);  c o = Note (C,o);  cs o = Note (Cs,o)
> df o = Note (Df,o);  d o = Note (D,o);  ds o = Note (Ds,o)
> ef o = Note (Ef,o);  e o = Note (E,o);  es o = Note (Es,o)
> ff o = Note (Ff,o);  f o = Note (F,o);  fs o = Note (Fs,o)
> gf o = Note (Gf,o);  g o = Note (G,o);  gs o = Note (Gs,o)
> af o = Note (Af,o);  a o = Note (A,o);  as o = Note (As,o)
> bf o = Note (Bf,o);  b o = Note (B,o);  bs o = Note (Bs,o)
>
> bn, wn, hn, qn, en, sn, tn, sfn    :: Dur
> dwn, dhn, dqn, den, dsn, dtn       :: Dur
> ddhn, ddqn, dden                   :: Dur
>
> bnr, wnr, hnr, qnr, enr, snr, tnr  :: Music
> dwnr, dhnr, dqnr, denr, dsnr, dtnr :: Music
> ddhnr, ddqnr, ddenr                :: Music
>
> bn  = 2         ; bnr  = Rest bn      -- brevis rest
> wn  = 1         ; wnr  = Rest wn      -- whole note rest
> hn  = 1%2       ; hnr  = Rest hn      -- half note rest
> qn  = 1%4       ; qnr  = Rest qn      -- quarter note rest
> en  = 1%8       ; enr  = Rest en      -- eight note rest
> sn  = 1%16      ; snr  = Rest sn      -- sixteenth note rest
> tn  = 1%32      ; tnr  = Rest tn      -- thirty-second note rest
> sfn = 1%64      ; sfnr = Rest sfn     -- sixty-fourth note rest
>
> dwn = 3%2       ; dwnr = Rest dwn     -- dotted whole note rest
> dhn = 3%4       ; dhnr = Rest dhn     -- dotted half note rest
> dqn = 3%8       ; dqnr = Rest dqn     -- dotted quarter note rest
> den = 3%16      ; denr = Rest den     -- dotted eighth note rest
> dsn = 3%32      ; dsnr = Rest dsn     -- dotted sixteenth note rest
> dtn = 3%64      ; dtnr = Rest dtn     -- dotted thirty-second note rest
>
> ddhn = 7%8      ; ddhnr = Rest ddhn   -- double-dotted half note rest
> ddqn = 7%16     ; ddqnr = Rest ddqn   -- double-dotted quarter note rest
> dden = 7%32     ; ddenr = Rest dden   -- double-dotted eighth note rest

\end{verbatim}}
\caption{Convenient note names and pitch conversion functions.}
\label{note-names}
\end{figure}

\subsection{Some Simple Examples}
\label{basic-examples}

With this modest beginning, we can already express quite a few musical
relationships simply and effectively.  

\paragraph*{Lines and Chords.}  

Two common ideas in music are the construction of notes in a
horizontal fashion (a {\em line} or {\em melody}), and in a vertical
fashion (a {\em chord}):
\begin{verbatim}

> line, chord :: [Music] -> Music
> line  = foldr1 (:+:)
> chord = foldr1 (:=:)

\end{verbatim}
From the notes in the C major triad in register 4, I can now construct
a C major arpeggio and chord as well:
\begin{verbatim} 

> cMaj = [ n 4 qn [] | n <- [c,e,g] ]  -- octave 4, quarter notes
>
> cMajArp = line  cMaj
> cMajChd = chord cMaj

\end{verbatim} 

\paragraph*{Delay and Repeat.}  

Suppose now that we wish to describe a melody {\tt m} accompanied by
an identical voice a perfect 5th higher.  In Haskore we simply write
``{\tt m :=:\ Trans 7 m}.''  Similarly, a canon-like structure
involving {\tt m} can be expressed as ``{\tt m :=:\ delay d m},''
where:
\begin{verbatim} 

> delay :: Dur -> Music -> Music
> delay d m = Rest d :+: m

\end{verbatim} 

Of course, Haskell's non-strict semantics also allows us to define
infinite musical objects.  For example, a musical object may be
repeated {\em ad nauseum} using this simple function:
\begin{verbatim} 

> repeatM :: Music -> Music
> repeatM m = m :+: repeatM m

\end{verbatim}
Thus an infinite ostinato can be expressed in this way, and then used
in different contexts that extract only the portion that's actually
needed.

\paragraph*{Inversion and Retrograde.}

The notions of inversion, retrograde, retrograde inversion, etc. used
in 12-tone theory are also easily captured in Haskore.  First let's
define a transformation from a line created by {\tt line} to a list:
\begin{verbatim}

> lineToList :: Music -> [Music]
> lineToList n@(Rest 0) = []
> lineToList (n :+: ns) = n : lineToList ns
>
> retro, invert, retroInvert, invertRetro :: Music -> Music
> retro    = line . reverse . lineToList
> invert m = line (map inv l)
>   where l@(Note r _ _: _)  = lineToList m
>         inv (Note p d nas) = Note (pitch (2*(absPitch r) - absPitch p)) d nas
>         inv (Rest d)       = Rest d
> retroInvert = retro  . invert
> invertRetro = invert . retro

\end{verbatim} 

\begin{exercise} Show that ``{\tt retro\ .\ retro},''
``{\tt invert\ .\ invert},'' and ``{\tt retroInvert\ .\ invertRetro}''
are the identity on values created by {\tt line}.
\end{exercise}

\begin{figure*}
\centerline{
\epsfysize=2.0in 
\epsfbox{Pics/poly.eps}
}
\caption{Nested Polyrhythms}
\label{polyrhythms}
\end{figure*}

\paragraph*{Polyrhythms.}

For some rhythmical ideas, consider first a simple {\em triplet} of
eighth notes; it can be expressed as ``{\tt Tempo (3\%2) m},'' where
{\tt m} is a line of three eighth notes.  In fact {\tt Tempo} can be
used to create quite complex rhythmical patterns.  For example,
consider the ``nested polyrhythms'' shown in Figure \ref{polyrhythms}.
They can be expressed quite naturally in Haskore as follows (note the
use of the {\em where} clause in {\tt pr2} to capture recurring
phrases):
\begin{verbatim} 

> pr1, pr2 :: Pitch -> Music
> pr1 p = Tempo (5%6) (Tempo (4%3) (mkLn 1 p qn :+:
>                               Tempo (3%2) (mkLn 3 p en :+:
>                                          mkLn 2 p sn :+:
>                                          mkLn 1 p qn    ) :+:
>                               mkLn 1 p qn) :+:
>                    Tempo (3%2) (mkLn 6 p en))
>
> pr2 p = Tempo (7%6) (m1 :+:
>                    Tempo (5%4) (mkLn 5 p en) :+:
>                    m1 :+:
>                    mkLn 2 p en)
>         where m1 = Tempo (5%4) (Tempo (3%2) m2 :+: m2)
>               m2 = mkLn 3 p en
>
> mkLn n p d = line (take n (repeat (Note p d [])))

\end{verbatim}
To play polyrhythms {\tt pr1} and {\tt pr2} in parallel using middle C
and middle G, respectively, we would do the following (middle C is in
the 5th octave):
\begin{verbatim} 

> pr12 :: Music
> pr12 = pr1 (C,5) :=: pr2 (G,5)

\end{verbatim} 

\paragraph*{Symbolic Meter Changes}

We can implement a notion of ``symbolic meter changes'' of the form
``oldnote = newnote'' (quarter note = dotted eighth, for example) by
defining a function:
\begin{verbatim}

> (=:=) :: Dur -> Dur -> Music -> Music
> old =:= new  =  Tempo (new/old)

\end{verbatim}
Of course, using the new function is not much longer than using {\tt
Tempo} directly, but it may have nemonic value.

\paragraph*{Determining Duration}

It is sometimes desirable to compute the duration in beats of a
musical object; we can do so as follows:
\begin{verbatim}

> dur :: Music -> Dur
>
> dur (Note _ d _)  = d
> dur (Rest d)      = d
> dur (m1 :+: m2)   = dur m1   +   dur m2
> dur (m1 :=: m2)   = dur m1 `max` dur m2
> dur (Tempo  a  m) = dur m / a
> dur (Trans  _  m) = dur m
> dur (Instr  _  m) = dur m
> dur (Player _  m) = dur m
> dur (Phrase _  m) = dur m

\end{verbatim} 

\paragraph*{Super-retrograde.}

Using {\tt dur} we can define a function {\tt revM} that reverses any
{\tt Music} value (and is thus considerably more useful than {\tt
retro} defined earlier).  Note the tricky treatment of {\tt (:=:)}.
\begin{verbatim}

> revM :: Music -> Music
> revM n@(Note _ _ _) = n
> revM r@(Rest _)     = r
> revM (Tempo a  m)   = Tempo a    (revM m)
> revM (Trans i  m)   = Trans i    (revM m)
> revM (Instr i  m)   = Instr i    (revM m)
> revM (Phrase pas m) = Phrase pas (revM m)
> revM (m1 :+: m2)    = revM m2 :+: revM m1
> revM (m1 :=: m2)    = 
>   let d1 = dur m1
>       d2 = dur m2
>   in if d1>d2 then revM m1 :=: (Rest (d1-d2) :+: revM m2)
>               else (Rest (d2-d1) :+: revM m1) :=: revM m2

\end{verbatim} 

\paragraph*{Truncating Parallel Composition}

Note that the duration of {\tt m1 :=: m2} is the maximum of the
durations of {\{\tt m1} and {\tt m2} (and thus if one is infinite, so
is the result).  Sometimes we would rather have the result be of
duration equal to the shorter of the two.  This is not as easy as it
sounds, since it may require interrupting the longer one in the middle
of a note (or notes).

We will define a ``truncating parallel composition'' operator {\tt
(/=)}, but first we will define an auxiliary function {\tt cut} such
that {\tt cut d m} is the musical object {\tt m} ``cut short'' to have
at most duration {\tt d}:
\begin{verbatim}

> cut :: Dur -> Music -> Music
> cut newDur m | newDur <= 0   = Rest 0
> cut newDur (Note x oldDur y) = Note x (min oldDur newDur) y
> cut newDur (Rest oldDur)     = Rest (min oldDur newDur)
> cut newDur (m1 :=: m2)       = cut newDur m1 :=: cut newDur m2
> cut newDur (m1 :+: m2)       = let m1' = cut newDur m1
>                                    m2' = cut (newDur - dur m1') m2
>                                in m1' :+: m2'
> cut newDur (Tempo  a m)      = Tempo a  (cut (newDur * a) m)
> cut newDur (Trans  a m)      = Trans a  (cut newDur m)
> cut newDur (Instr  a m)      = Instr a  (cut newDur m)
> cut newDur (Player a m)      = Player a (cut newDur m)
> cut newDur (Phrase a m)      = Phrase a (cut newDur m)

\end{verbatim}
Note that {\tt cut} is equipped to handle a {\tt Music} object of
infinite length.

With {\tt cut}, the definition of {\tt (/=:)} is now straightforward:
\begin{verbatim}

> (/=:) :: Music -> Music -> Music
> m1 /=: m2 = cut (min (dur m1) (dur m2)) (m1 :=: m2)

\end{verbatim}
Unfortunately, whereas {\tt cut} can handle infinite-duration music
values, {\tt (/=:)} cannot.

\begin{exercise} 
Define a version of {\tt (/=:)} that shortens correctly when either or
both of its arguments are infinite in duration.
\end{exercise}

\paragraph*{Trills}

A {\em trill} is an ornament that alternates rapidly between two
(usually adjacent) pitches.  We will define two versions of a trill
function, both of which take the starting note and an interval for the
trill note as arguments (the interval is usually one or two, but can
actually be anything).  One version will additionally have an argument
that specifies how long each trill note should be, whereas the other
will have an argument that specifies how many trills should occur.
In both cases the total duration will be the same as the duration of
the original note.

Here is the first trill function:
\begin{verbatim}

> trill :: Int -> Dur -> Music -> Music
> trill i sDur (Note p tDur x) =
>       if sDur >= tDur then Note p tDur x
>       else Note p sDur x
>            :+: trill (negate i) sDur (Note (trans i p) (tDur-sDur) x)
> trill i d (Tempo  a m) = Tempo  a (trill i (d*a) m)
> trill i d (Trans  a m) = Trans  a (trill i d m)
> trill i d (Instr  a m) = Instr  a (trill i d m)
> trill i d (Player a m) = Player a (trill i d m)
> trill i d (Phrase a m) = Phrase a (trill i d m)
> trill _ _ _ = error "Trill input must be a single note."

\end{verbatim}
It is simple to define a version of this function that starts on the
trill note rather than the start note:
\begin{verbatim}

> trill' :: Int -> Dur -> Music -> Music
> trill' i sDur m =
>       trill (negate i) sDur (Trans i m)

\end{verbatim}

The second way to define a trill is in terms of the number of
subdivided notes to be included in the trill.  We can use the first
trill function to define this new one:
\begin{verbatim}

> trilln :: Int -> Int -> Music -> Music
> trilln i nTimes m =
>       trill i (dur m / (nTimes%1)) m

\end{verbatim}
This, too, can be made to start on the other note.
\begin{verbatim}

> trilln' :: Int -> Int -> Music -> Music
> trilln' i nTimes m =
>       trilln (negate i) nTimes (Trans i m)

\end{verbatim}

Finally, a {\tt roll} can be implemented as a trill whose interval is
zero.  This feature is particularly useful for percussion.
\begin{verbatim}

> roll  :: Dur -> Music -> Music
> rolln :: Int -> Music -> Music
>
> roll  dur    m = trill  0 dur m
> rolln nTimes m = trilln 0 nTimes m

\end{verbatim}

\paragraph*{Percussion}

Percussion is a difficult notion to represent in the abstract, since
in a way, a percussion instrument is just another instrument, so why
should it be treated differently?  On the other hand, even common
practice notation treats it specially, even though it has much in
common with non-percussive notation.  The midi standard is equally
ambiguous about the treatment of percussion: on one hand, percussion
sounds are chosen by specifying an octave and pitch, just like any
other instrument, on the other hand these notes have no tonal meaning
whatsoever: they are just a conveneient way to select from a large
number of percussion sounds.  Indeed, part of the General Midi
Standard is a set of names for commonly used percussion sounds.

\begin{figure}{\small
\begin{verbatim} 

> data PercussionSound =
>         AcousticBassDrum  -- Midi Key 35
>       | BassDrum1         -- Midi Key 36
>       | SideStick         -- ...
>       | AcousticSnare | HandClap      | ElectricSnare | LowFloorTom
>       | ClosedHiHat   | HighFloorTom  | PedalHiHat    | LowTom
>       | OpenHiHat     | LowMidTom     | HiMidTom      | CrashCymbal1
>       | HighTom       | RideCymbal1   | ChineseCymbal | RideBell
>       | Tambourine    | SplashCymbal  | Cowbell       | CrashCymbal2
>       | Vibraslap     | RideCymbal2   | HiBongo       | LowBongo
>       | MuteHiConga   | OpenHiConga   | LowConga      | HighTimbale
>       | LowTimbale    | HighAgogo     | LowAgogo      | Cabasa
>       | Maracas       | ShortWhistle  | LongWhistle   | ShortGuiro
>       | LongGuiro     | Claves        | HiWoodBlock   | LowWoodBlock
>       | MuteCuica     | OpenCuica     | MuteTriangle
>       | OpenTriangle      -- Midi Key 82
>    deriving (Show,Eq,Ord,Ix,Enum)

\end{verbatim}}
\caption{General Midi Percussion Names}
\label{fig:percussion}
\end{figure}

Since Midi is such a popular platform, we can at least define some
handy functions for using the General Midi Standard.  We start by
defining the datatype shown in Figure \ref{fig:percussion}, which
borrows its constructor names from the General Midi standard.  The
comments reflecting the ``Midi Key'' numbers will be explained later,
but basically a Midi Key is the equivalent of an absolute pitch in
Haskore terminology.  So all we need is a way to convert these
percussion sound names into a {\tt Music} object; i.e.\ a {\tt Note}:
\begin{verbatim} 

> perc :: PercussionSound -> Dur -> [NoteAttribute] -> Music
> perc ds dur na = Note (pitch (fromEnum ds + 35)) dur na

\end{verbatim}

For example, here are eight bars of a simple rock or "funk groove"
that uses {\tt perc} and {\tt roll}:
\begin{verbatim} 

> funkGroove
>   = let p1 = perc LowTom        qn []
>         p2 = perc AcousticSnare en []
>     in Tempo 3 (Instr "Drums" (cut 8 (repeatM
>          ( (p1 :+: qnr :+: p2 :+: qnr :+: p2 :+:
>             p1 :+: p1 :+: qnr :+: p2 :+: enr)
>            :=: roll en (perc ClosedHiHat 2 []) )
>        )))

\end{verbatim}
\out{
We can go one step further by defining our own little ``percussion
datatype:'' 
\begin{verbatim} 

  data Percussion = Perc      Dur [NoteAttribute] -- percussion
                  | Pause     Dur                 -- rest
                  | Roll  Dur Dur [NoteAttribute] -- roll w/duration
                  | Rolln Int Dur [NoteAttribute] -- roll w/number of strokes

\end{verbatim}
whose interpretation is given by:
\begin{verbatim} 

  percLine :: PercussionSound -> [Percussion] -> Music
  percLine dsnd l = Instr "Drums" (foldr (dlAux dsnd) (Rest 0) l) where
    dlAux dsnd (N dur na)            = perc dsnd dur na :+: xs
    dlAux dsnd (R dur)               = Rest dur :+: xs
    dlAux dsnd (Roll sDur dur na)    = roll sDur (perc dsnd dur na) :+: xs
    dlAux dsnd (Rolln nTimes dur na) = rolln nTimes (perc dsnd dur na)
                                                :+: dlAux dsnd xs

\end{verbatim} 
}

\begin{exercise}
Find a simple piece of music written by your favorite composer, and
transcribe it into Haskore.  In doing so, look for repeating patterns,
transposed phrases, etc. and reflect this in your code, thus revealing
deeper structural aspects of the music than that found in common
practice notation.
\end{exercise}

Appendix \ref{chick} shows the first 28 bars of Chick Corea's
``Children's Song No.~6'' encoded in Haskore.

\subsection{Phrasing and Articulation}
\label{phrasing}

Recall that the {\tt Note} constructor contained a field of {\tt
NoteAttributes}.  These are values that are attached to notes for the
purpose of notation or musical interpretation.  Likewise, the {\tt
Phrase} constructor permits one to annotate an entire musical object
with {\tt PhraseAttributes}.  These two attribute datatypes cover a
wide range of attributions found in common practice notation, and are
shown in Figure \ref{attributes}.  Beware that use of them requires
the use of a player that knows how to interpret them!  Players will be
described in more detail in Section \ref{players}.

\begin{figure}
\begin{verbatim} 

> data NoteAttribute = Volume Float        -- by convention: 0=min, 100=max
>                    | Fingering Int
>                    | Dynamics String
>                    | PFields [Float]
>      deriving (Eq, Show)
>
> data PhraseAttribute = Dyn Dynamic
>                      | Art Articulation
>                      | Orn Ornament
>      deriving (Eq, Show)
>
> data Dynamic = Accent Float | Crescendo Float | Diminuendo Float
>              | PPP | PP | P | MP | SF | MF | NF | FF | FFF | Loudness Float
>              | Ritardando Float | Accelerando Float
>      deriving (Eq, Show)
>
> data Articulation = Staccato Float | Legato Float | Slurred Float
>                   | Tenuto | Marcato | Pedal | Fermata | FermataDown | Breath
>                   | DownBow | UpBow | Harmonic | Pizzicato | LeftPizz 
>                   | BartokPizz | Swell | Wedge | Thumb | Stopped
>      deriving (Eq, Show)
>
> data Ornament = Trill | Mordent | InvMordent | DoubleMordent
>               | Turn | TrilledTurn | ShortTrill
>               | Arpeggio | ArpeggioUp | ArpeggioDown
>               | Instruction String | Head NoteHead
>      deriving (Eq, Show)
>
> data NoteHead = DiamondHead | SquareHead | XHead | TriangleHead
>               | TremoloHead | SlashHead | ArtHarmonic | NoHead
>      deriving (Eq, Show)

\end{verbatim}
\caption{Note and Phrase Attributes.}
\label{attributes}
\end{figure}

Note that some of the attributes are parameterized with a numeric
value.  This is used by a player to control the degree to which
an articulation is to be applied.  For example, we would expect {\tt
Legato 1.2} to create more of a legato feel than {\tt Legato 1.1}.
The following constants represent default values for some of the
parameterized attributes: 
\begin{verbatim}

> legato, staccato  :: Articulation
> accent, bigAccent :: Dynamic
>
> legato    = Legato 1.1
> staccato  = Staccato 0.5
> accent    = Accent 1.2
> bigAccent = Accent 1.5

\end{verbatim} 

To understand exactly how a player interprets an attribute requires
knowing how players are defined.  Haskore defines only a few simple
players, so in fact many of the attributes in Figure \ref{attributes}
are to allow the user to give appropriate interpretations of them by
her particular player.  But before looking at the structure of players
we will need to look at the notion of a {\em performance} (these two
ideas are tightly linked, which is why the {\tt Players} and {\tt
Performance} modules are mutually recursive).
