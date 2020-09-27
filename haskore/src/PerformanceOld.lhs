\section{Interpretation and Performance}
\label{performance}

\begin{verbatim} 

> module Performance (module Performance, module Basics) -- module Players
>        where
>
> import Basics
> -- import Players

\end{verbatim} 

Now that we have defined the structure of musical objects, let us turn
to the issue of {\em performance}, which we define as a temporally
ordered sequence of musical {\em events}:
\begin{verbatim}

> type Performance = [Event]
>
> data Event = Event {eTime :: Time, eInst :: IName, ePitch :: AbsPitch,
>                     eDur  :: DurT, eVol  :: Volume, pFields :: [Float]}
>      deriving (Eq,Ord,Show)
>
> type Time      = Float
> type DurT      = Float
> type Volume    = Float

\end{verbatim} 
An event is the lowest of our music representations not yet committed
to Midi, csound, or the MusicKit.  An event {\tt Event \{eTime = s,
eInst = i, ePitch = p, eDur = d, eVol = v\}} captures the fact that at 
start time {\tt s}, instrument {\tt i} sounds pitch {\tt p} with volume
{\tt v} for a duration {\tt d} (where now duration is measured in seconds,
rather than beats).

To generate a complete performance of, i.e.\ give an interpretation
to, a musical object, we must know the time to begin the performance,
and the proper volume, key and tempo.  We must also know what {\em
players} to use; that is, we need a mapping from the {\tt PName}s in
an abstract musical object to the actual players to be used.  (We
don't yet need a mapping from abstract {\tt INames} to instruments,
since this is handled in the translation from a performance into, say,
Midi, such as defined in Section \ref{midi}.)

We can thus model a performer as a function {\tt perform} which maps
all of this information and a musical object into a performance:
\begin{verbatim}

> perform :: PMap -> Context -> Music -> Performance
>
> type PMap    = PName -> Player
> data Context = Context {cTime :: Time, cPlayer :: Player, cInst :: IName,
>                         cDur  :: DurT, cKey    :: Key,    cVol  :: Volume}
>      deriving Show
>
> type Key     = AbsPitch

 perform pmap c@Context {cTime = t, cPlayer = pl, cDur = dt, cKey = k} m =
  case m of
     Note p d nas -> playNote pl c p d nas
     Rest d       -> []
     m1 :+: m2    -> perform pmap c m1 ++ 
                     perform pmap (c {cTime = t + dur m1 * dt}) m2
     m1 :=: m2    -> merge (perform pmap c m1) (perform pmap c m2)
     Tempo  a   m -> perform pmap (c {cDur = dt / rtof a}) m
     Trans  p   m -> perform pmap (c {cKey = k + p}) m
     Instr  nm  m -> perform pmap (c {cInst = nm}) m
     Player nm  m -> perform pmap (c {cPlayer = pmap nm}) m
     Phrase pas m -> interpPhrase pl pmap c pas m

\end{verbatim} 

\begin{figure}
\begin{verbatim} 

> perform pmap c m = fst (perf pmap c m)
>
> perf :: PMap -> Context -> Music -> (Performance, DurT)
>
> perf pmap c@Context {cTime = t, cPlayer = pl, cDur = dt, cKey = k} m =
>   case m of
>     Note p d nas -> (playNote pl c p d nas, rtof d *dt)
>     Rest d       -> ([], rtof d *dt)
>     m1 :+: m2    -> let (pf1,d1) = perf pmap c m1
>                         (pf2,d2) = perf pmap (c {cTime = t+d1}) m2
>                     in (pf1++pf2, d1+d2)
>     m1 :=: m2    -> let (pf1,d1) = perf pmap c m1
>                         (pf2,d2) = perf pmap c m2
>                     in (merge pf1 pf2, max d1 d2)
>     Tempo  a   m -> perf pmap (c {cDur = dt / rtof a}) m
>     Trans  p   m -> perf pmap (c {cKey = k + p}) m
>     Instr  nm  m -> perf pmap (c {cInst = nm}) m
>     Player nm  m -> perf pmap (c {cPlayer = pmap nm}) m
>     Phrase pas m -> interpPhrase pl pmap c pas m

\end{verbatim} 
\caption{The ``real'' {\tt perform} function.}
\label{real-perform}
\end{figure}

Some things to note:
\begin{enumerate} 
\item
The {\tt Context} is the running ``state'' of the performance, and
gets updated in several different ways.  For example, the
interpretation of the {\tt Tempo} constructor involves scaling {\tt
dt} appropriately and updating the {\tt DurT} field of the context.

\item
Interpretation of notes and phrases is player dependent.  Ultimately a
single note is played by the {\tt playNote} function, which takes the
player as an argument.  Similarly, phrase interpretation is also
player dependent, reflected in the use of {\tt interpPhrase}.
Precisely how these two functions work is described in Section
\ref{players}.

\item
The {\tt DurT} component of the context is the duration, in seconds,
of one whole note.  To make it easier to compute, we can define a
``metronome'' function that, given a standard metronome marking (in
beats per minute) and the note type associated with one beat (quarter
note, eighth note, etc.)  generates the duration of one whole note:
\begin{verbatim} 

> metro :: Float -> Dur -> DurT
> metro setting dur = 60 / (setting * rtof dur)

\end{verbatim} 
Thus, for example, {\tt metro 96 qn} creates a tempo of 96 quarter
notes per minute.

\item
In the treatment of {\tt (:+:)}, note that the sub-sequences are
appended together, with the start time of the second argument delayed
by the duration of the first.  The function {\tt dur} (defined in
Section \ref{basic-examples}) is used to compute this duration.  Note
that this results in a quadratic time complexity for {\tt perform}.  A
more efficient solution is to have {\tt perform} compute the duration
directly, returning it as part of its result.  This version of {\tt
perform} is shown in Figure \ref{real-perform}.

\item
In contrast, the sub-sequences derived from the arguments to {\tt
(:=:)} are merged into a time-ordered stream.  The definition of {\tt
merge} is given below.
\end{enumerate} 
\begin{verbatim} 

> merge :: Performance -> Performance -> Performance

  merge a@(e1:es1) b@(e2:es2) = 
        if e1 < e2 then e1 : merge es1 b
                   else e2 : merge a es2
  merge [] es2 = es2
  merge es1 [] = es1

\end{verbatim} 
Note that {\tt merge} compares entire events rather than just start
times.  This is to ensure that it is commutative, a desirable
condition for some of the proofs used in Section \ref{equivalence}.
Here is a more efficient version that will work just as well in
practice:
\begin{verbatim}

> merge a@(e1:es1) b@(e2:es2) =
>   if eTime e1 < eTime e2 then e1 : merge es1 b
>                          else e2 : merge a es2
> merge [] es2 = es2
> merge es1 [] = es1

\end{verbatim} 

\section{Players}
\label{players}

\begin{verbatim} 

  module Players (module Players, module Music, module Performance)
         where

  import Music
  import Performance

\end{verbatim} 

In the last section we saw how a performance involved the notion of a
{\em player}.  The reason for this is the same as for real players and
their instruments: many of the note and phrase attributes (see Section
\ref{phrasing}) are player and instrument dependent.  For example, how
should ``legato'' be interpreted in a performance?  Or ``diminuendo?''
Different players interpret things in different ways, of course, but
even more fundamental is the fact that a pianist, for example,
realizes legato in a way fundamentally different from the way a
violinist does, because of differences in their instruments.
Similarly, diminuendo on a piano and a harpsichord are different
concepts.  

With a slight stretch of the imagination, we can even consider a
``notator'' of a score as a kind of player: exactly how the music is
rendered on the written page may be a personal, stylized process.  For
example, how many, and which staves should be used to notate a
particular instrument?

In any case, to handle these issues, Haskore has a notion of a {\em
player} which ``knows'' about differences with respect to performance
and notation.  A Haskore player is a 4-tuple consisting of a name and
three functions: one for interpreting notes, one for phrases, and one
for producing a properly notated score.  
\begin{verbatim}

> data Player = MkPlayer { pName :: PName, 
>                          playNote :: NoteFun,
>                          interpPhrase :: PhraseFun, 
>                          notatePlayer :: NotateFun }
>      deriving Show

> type NoteFun   = 
>      Context -> Pitch -> Dur -> [NoteAttribute] -> Performance
> type PhraseFun = 
>      PMap -> Context -> [PhraseAttribute] -> Music -> (Performance,DurT)
> type NotateFun = ()

\end{verbatim} The last line above is because notation is currently
not implemented.  Note that both {\tt NoteFun} and {\tt PhraseFun}
functions return a {\tt Performance} (imported from module {\tt
Perform}).

\begin{figure}
\begin{verbatim} 

> defPlayer :: Player
> defPlayer  = MkPlayer { pName        = "Default",
>                         playNote     = defPlayNote defNasHandler,
>                         interpPhrase = defInterpPhrase defPasHandler,
>                         notatePlayer = defNotatePlayer ()            }
>
> defPlayNote :: (Context->NoteAttribute->Event->Event) -> NoteFun
> defPlayNote nasHandler 
>   c@(Context cTime cPlayer cInst cDur cKey cVol) p d nas =
>        [ foldr (nasHandler c)
>                (Event {eTime  = cTime, eInst  = cInst,
>                        ePitch = absPitch p + cKey,
>                        eDur   = rtof d * cDur, eVol = cVol})
>                nas ]
>
> defNasHandler :: Context-> NoteAttribute -> Event -> Event
> defNasHandler c (Volume v) ev = ev {eVol = (cVol c + v)/2}
> defNasHandler _              _          ev = ev
>
> defInterpPhrase :: (PhraseAttribute->Performance->Performance) -> PhraseFun
> defInterpPhrase pasHandler pmap context pas m =
>        let (pf,dur) = perf pmap context m
>        in  (foldr pasHandler pf pas, dur)
>
> defPasHandler :: PhraseAttribute -> Performance -> Performance
> defPasHandler (Dyn (Accent x))   pf = map (\e -> e {eVol = x * eVol e}) pf
> defPasHandler (Art (Staccato x)) pf = map (\e -> e {eDur = x * eDur e}) pf
> defPasHandler (Art (Legato   x)) pf = map (\e -> e {eDur = x * eDur e}) pf
> defPasHandler _                  pf = pf
>
> defNotatePlayer   :: () -> NotateFun
> defNotatePlayer _ = ()

\end{verbatim} 
\caption{Definition of default Player {\tt defPlayer}.}
\label{default-Player}
\end{figure}

\subsection{Examples of Player Construction}

A ``default player'' called {\tt defPlayer} (not to be confused with
``deaf player''!) is defined for use when none other is specified in
the score; it also functions as a base from which other players can be
derived.  {\tt defPlayer} responds only to the {\tt Volume} note
attribute and to the {\tt Accent}, {\tt Staccato}, and {\tt Legato}
phrase attributes.  It is defined in Figure \ref{default-Player}.
Before reading this code, recall how players are invoked by the {\tt
perform} function defined in the last section; in particular, note the
calls to {\tt playNote} and {\tt interpPhase} defined above.  Then
note:
\begin{enumerate} 
\item {\tt defPlayNote} is the only function (even in the definition
of {\tt perform}) that actually generates an event.  It also modifies
that event based on an interpretation of each note attribute by the
function {\tt defHasHandler}.

\item  {\tt defNasHandler} only recognizes the {\tt Volume} attribute,
which it uses to set the event volume accordingly.

\item {\tt defInterpPhrase} calls (mutually recursively) {\tt
perform} to interpret a phrase, and then modifies the result based on
an interpretation of each phrase attribute by the function {\tt
defPasHandler}.

\item {\tt defPasHandler} only recognizes the {\tt Accent}, {\tt
Staccato}, and {\tt Legato} phrase attributes.  For each of these it
uses the numeric argument as a ``scaling'' factor of the volume (for
{\tt Accent}) and duration (for {\tt Staccato} and {\tt Lagato}).
Thus {\tt (Phrase [Legato 1.1] m)} effectively increases the duration
of each note in {\tt m} by 10\% (without changing the tempo).
\end{enumerate} 

It should be clear that much of the code in Figure
\ref{default-Player} can be re-used in defining a new player.
For example, to define a player {\tt weird} that interprets note
attributes just like {\tt defPlayer} but behaves differently with
respect to phrase attributes, we could write:
\begin{verbatim} 
  weird :: Player
  weird  = MkPlayer { pname        = "Weirdo",
                      playNote     = defPlayNote defNasHandler,
                      interpPhrase = defInterpPhrase myPasHandler
                      notatePlayer = defNotatePlayer ()           }
\end{verbatim} 
and then supply a suitable definition of {\tt myPasHandler}.  That
definition could also re-use code, in the following sense: suppose we
wish to add an interpretation for {\tt Crescendo}, but otherwise
have {\tt myPasHandler} behave just like {\tt defPasHandler}.
\begin{verbatim} 
  myPasHandler :: PhraseAttribute -> Performance -> Performance
  myPasHandler (Dyn (Crescendo x)) pf = ...
  myPasHandler  pa                 pf = defPasHandler pa pf
\end{verbatim} 

\begin{exercise}
Fill in the {\tt ...} in the definition of {\tt myPasHandler} according
to the following strategy:  Assume $0<{\tt x}<1$.  Gradually scale
the volume of each event by a factor of $1.0$ through $1.0+{\tt x}$,
using linear interpolation.
\end{exercise}

\begin{exercise} 
Choose some of the other phrase attributes and provide interpretations
of them, such as {\tt Diminuendo}, {\tt Slurred}, {\tt Trill}, etc.
(The {\tt trill} functions from section \ref{basic-examples} may be
useful here.)
\end{exercise}

Figure \ref{fancy-Player} defines a relatively sophisticated player
called {\tt fancyPlayer} that knows all that {\tt defPlayer} knows, and
much more.  Note that {\tt Slurred} is different from {\tt Legato} in
that it doesn't extend the duration of the {\em last} note(s).  The
behavior of ${\tt (Ritardando }\ x{\tt )}$ can be explained as
follows.  We'd like to ``stretch'' the time of each event by a factor
from $0$ to $x$, linearly interpolated based on how far along the
musical phrase the event occurs.  I.e., given a start time $t_0$ for
the first event in the phrase, total phrase duration $D$, and event
time $t$, the new event time $t'$ is given by:
\[ t'   = (1 + \frac{t-t_0}{D}x)(t-t_0) + t_0 \]
Further, if $d$ is the duration of the event, then the end of
the event $t+d$ gets stretched to a new time $t_d'$ given by:
\[ t_d' = (1 + \frac{t+d-t_0}{D}x)(t+d-t_0) + t_0 \]
The difference $t_d' - t'$ gives us the new, stretched duration $d'$,
which after simplification is:
\[ d' = (1 + \frac{2(t-t_0)+d}{D}x)d \]
{\tt Accelerando} behaves in exactly the same way, except that it
shortens event times rather than lengthening them.  And, a similar but
simpler strategy explains the behaviors of {\tt Crescendo} and {\tt
Diminuendo}.

\begin{figure}\small
\begin{verbatim} 

> fancyPlayer :: Player
> fancyPlayer  = MkPlayer { pName        = "Fancy",
>                           playNote     = defPlayNote defNasHandler,
>                           interpPhrase = fancyInterpPhrase,
>                           notatePlayer = defNotatePlayer ()        }
> fancyInterpPhrase :: PhraseFun
> fancyInterpPhrase pmap c [] m = perf pmap c m
> fancyInterpPhrase pmap c@Context {cTime = t, cPlayer = pl, cInst = i, 
>                                   cDur = dt, cKey = k,     cVol = v}
>                   (pa:pas) m =
>  let pfd@(pf,dur) = fancyInterpPhrase pmap c pas m
>      loud x       = fancyInterpPhrase pmap c (Dyn (Loudness x) : pas) m
>      stretch x = let t0 = eTime (head pf);  r  = x/dur
>                      upd (e@Event {eTime = t, eDur = d}) = 
>                          let dt = t-t0
>                              t' = (1+dt*r)*dt + t0
>                              d' = (1+(2*dt+d)*r)*d
>                          in e {eTime = t', eDur = d'}
>                  in (map upd pf, (1+x)*dur)
>      inflate x = let t0 = eTime (head pf);  r  = x/dur
>                      upd (e@Event {eTime = t, eVol = v}) = 
>                          e {eVol = (1+(t-t0)*r)*v}
>                  in (map upd pf, dur)
>  in case pa of
>     Dyn (Accent x)      -> (map (\e-> e {eVol = x * eVol e}) pf, dur)
>     Dyn PPP -> loud  40  ;  Dyn  PP -> loud  50  ;  Dyn   P -> loud  60
>     Dyn  MP -> loud  70  ;  Dyn  SF -> loud  80  ;  Dyn  MF -> loud  90
>     Dyn  NF -> loud 100  ;  Dyn  FF -> loud 110  ;  Dyn FFF -> loud 120
>     Dyn (Loudness x)    -> fancyInterpPhrase pmap c {cVol = x} pas m
>     Dyn (Crescendo x)   -> inflate   x ; Dyn (Diminuendo x)  -> inflate (-x)
>     Dyn (Ritardando  x) -> stretch   x ; Dyn (Accelerando x) -> stretch (-x)
>     Art (Staccato x)    -> (map (\e-> e {eDur = x * eDur e}) pf, dur)
>     Art (Legato   x)    -> (map (\e-> e {eDur = x * eDur e}) pf, dur)
>     Art (Slurred  x)    -> 
>         let lastStartTime = foldr (\e t -> max (eTime e) t) 0 pf
>             setDur e      = if eTime e < lastStartTime
>                             then e {eDur = x * eDur e}
>                             else e
>         in (map setDur pf, dur) 
>     Art _               -> pfd  -- Remaining articulations:
>                --   Tenuto | Marcato | Pedal | Fermata  | FermataDown
>                -- | Breath | DownBow | UpBow | Harmonic | Pizzicato
>                -- | LeftPizz | BartokPizz | Swell | Wedge | Thumb | Stopped
>     Orn _              -> pfd  -- Remaining ornamenations:
>                --   Trill | Mordent | InvMordent | DoubleMordent | Turn
>                -- | TrilledTurn | ShortTrill | Arpeggio | ArpeggioUp 
>                -- | ArpeggioDown | Instruction String | Head NoteHead
>     -- Design Bug: To do these right we need to keep the KEY SIGNATURE
>     -- around so that we can determine, for example, what the trill note is.
>     -- Alternatively, provide an argument to Trill to carry this info.

\end{verbatim} 
\caption{Definition of Player {\tt fancyPlayer}.}
\label{fancy-Player}
\end{figure}

