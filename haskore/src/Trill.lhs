\subsection{Trills}

\begin{verbatim}

> module Trill (module Trill, module Basics)
>	where
>
> import Basics

\end{verbatim}

A {\em trill} is an ornament that alternates rapidly between two (usually
adjacent) pitches.  Let's implement a trill as a function that take a note as
an argument and returns a series of notes whose durations add up to the same
duration as as the given note.

A trill alternates between the given note and another note, usually the note
above it in the scale.  Therefore, it must know what other note to use.  So
that the structure of {\tt trill} remains parallel across different keys, we'll
implement the other note in terms of its interval from the given note in half
steps.  Usually, the note is either a half-step above (interval = 1) or a
whole-step above (interval = 2).  Using negative numbers, a trill that goes to
lower notes can even be implemented.

Also, the trill needs to know how fast to alternate between the two notes. One
way is simply to specify the type of smaller note to use. (Another
implementation will be discussed later.)  So, our {\tt trill} has the
following type:
\begin{verbatim}

> trill :: Int -> Dur -> Music -> Music

\end{verbatim}
Its implementation:
\begin{verbatim}

> trill i sDur (Note p tDur x) =
>	if (sDur >= tDur) then Note p tDur x
>	else
>  	  (Note p sDur x) :+:
>	  trill (negate i) sDur (Note (trans i p) (tDur - sDur) x)
> trill i d (Tempo a m) = Tempo a (trill i (d * a) m)
> trill i d (Trans a m)   = Trans a (trill i d m)
> trill i d (Instr a m)	  = Instr a (trill i d m)
> trill i d (Player a m)  = Player a (trill i d m)
> trill i d (Phrase a m)  = Phrase a (trill i d m)
> trill _ _ _ = error "Trill input must be a single note."

\end{verbatim}
This {\tt trill} starts on the given note, rather than the second note.  It
is simple to define a function that starts on the other note:
\begin{verbatim}

> trill' :: Int -> Dur -> Music -> Music
> trill' i sDur (Note p tDur x) =
>	trill (negate i) sDur (Note (trans i p) tDur x)

\end{verbatim}
Another way to define a trill is in terms of the number of subdivided notes
to be included in the trill.  
\begin{verbatim}

> trilln :: Int -> Int -> Music -> Music
> trilln i nTimes (Note p dur x) =
>	trill i (dur / (nTimes%1)) (Note p dur x)

\end{verbatim}
This, too, can be made to start on the other note.
\begin{verbatim}

> trilln' :: Int -> Int -> Music -> Music
> trilln' i nTimes (Note p dur x) =
>	trilln (negate i) nTimes (Note (trans i p) dur x)

\end{verbatim}
A {\tt roll} can be implemented as a trill whose interval is zero.  This
feature could be especially useful for percussion.
\begin{verbatim}

> roll  :: Dur -> Music -> Music
> rolln :: Int -> Music -> Music
>
> roll  dur    m = trill  0 dur m
> rolln nTimes m = trilln 0 nTimes m

\end{verbatim}
