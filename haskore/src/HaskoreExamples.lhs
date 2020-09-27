\section{Examples of Haskore in Action}
\label{examples}

{\small\begin{verbatim} 

> module HaskoreExamples (module HaskoreExamples, module Haskore, 
>                         module ChildSong6, module SelfSim)
>        where
>
> import Haskore
> import System.IO
> import ChildSong6
> import SelfSim
> import Ssf
> import Data.Ratio

Simple examples of Haskore in action.  Note that this module also
imports modules ChildSong6, SelfSim, and Ssf.

-----------------------------------------------------------------------------

From the tutorial, try things such as pr12, cMajArp, cMajChd, etc. and
try applying inversions, retrogrades, etc. on the same examples.  Also
try "childSong6" imported from module ChildSong.  For example:

> t0 = test (Instr "piano" childSong6)

-----------------------------------------------------------------------------

C Major scale for use in examples below:

> cMajScale = Tempo 2
>             (line [c 4 en [], d 4 en [], e 4 en [], f 4 en [], 
>                    g 4 en [], a 4 en [], b 4 en [], c 5 en []])
>
> cms' = line [c 4 en [], d 4 en [], e 4 en [], f 4 en [], 
>              g 4 en [], a 4 en [], b 4 en [], c 5 en []]
>
> cms = cMajScale

Test of various articulations and dynamics:

> t1 = test (Instr "percussion"
>        (Phrase [Art (Staccato 0.1)] cms :+:
>         cms                             :+:
>         Phrase [Art (Legato   1.1)] cms    ))
>
> temp = Instr "piano" (Phrase [Dyn (Crescendo 4.0)] (c 4 en []))
>
> mu2 = Instr "vibes"
>        (Phrase [Dyn (Diminuendo 0.75)] cms :+:
>         Phrase [Dyn (Crescendo 4.0), Dyn (Loudness 25)] cms)
> t2 = test mu2
>
> t3 = test (Instr "flute" 
>        (Phrase [Dyn (Accelerando 0.3)] cms :+:
>         Phrase [Dyn (Ritardando  0.6)] cms    ))

-----------------------------------------------------------------------------

A function to recursively apply transformations f (to elements in a
sequence) and g (to accumulated phrases):

> rep :: (Music -> Music) -> (Music -> Music) -> Int -> Music -> Music
> rep f g 0 m = Rest 0
> rep f g n m = m :=: g (rep f g (n-1) (f m))

An example using "rep" three times, recursively, to create a "cascade"
of sounds.

> run       = rep (Trans 5) (delay tn) 8 (c 4 tn [])
> cascade   = rep (Trans 4) (delay en) 8 run
> cascades  = rep  id       (delay sn) 2 cascade
> t4' x     = test (Instr "piano" x)
> t4        = test (Instr "piano" 
>               (cascades :+: revM cascades))

What happens if we simply reverse the f and g arguments?

> run'      = rep (delay tn) (Trans 5) 4 (c 4 tn [])
> cascade'  = rep (delay en) (Trans 4) 6 run'
> cascades' = rep (delay sn)  id       2 cascade'
> t5        = test (Instr "piano" cascades')

-----------------------------------------------------------------------------

Example from the SelfSim module.

> t10s   = test (rep (delay durss) (Trans 4) 2 ss)

-----------------------------------------------------------------------------

Example from the ChildSong6 module.

> cs6 = test childSong6

-----------------------------------------------------------------------------

Example from the Ssf (Stars and Stripes Forever) module.

> ssf0 = test ssf

-----------------------------------------------------------------------------

Midi percussion test.  Plays all "notes" in a range.  (Requires adding
an instrument for percussion to the UserPatchMap.)

> drums a b = Instr "drums" 
>                   (line (map (\p-> Note (pitch p) sn []) [a..b]))
> t11 a b = test (drums a b)

-----------------------------------------------------------------------------

Test of cut and shorten.

> t12 = test (cut 4 childSong6)
> t12a = test (cms /=: childSong6)

-----------------------------------------------------------------------------

Tests of the trill functions.

> t13note = (Note (C,5) qn [])
> t13 =  test (trill   1 sn t13note)
> t13a = test (trill'  2 dqn t13note)
> t13b = test (trilln  1 5 t13note)
> t13c = test (trilln' 3 7 t13note)
> t13d = test (roll tn t13note)
> t13e = test (Tempo (2/3) (Trans 2 (Instr "piano" (trilln' 2 7 t13note))))

-----------------------------------------------------------------------------

Tests of drum.

> t14 = test (Instr "Drums" (perc AcousticSnare qn []))

> -- a "funk groove"
> t14b = let p1 = perc LowTom        qn []
>            p2 = perc AcousticSnare en []
>        in test (Tempo 3 (Instr "Drums" (cut 8 (repeatM
>                  ((p1 :+: qnr :+: p2 :+: qnr :+: p2 :+:
>                    p1 :+: p1 :+: qnr :+: p2 :+: enr)
>                   :=: roll en (perc ClosedHiHat 2 []))))))

> -- a "jazz groove"
> t14c = let p1 = perc CrashCymbal2  qn []
>            p2 = perc AcousticSnare en []
>            p3 = perc LowTom        qn []
>        in test (Tempo 3 (Instr "Drums" (cut 4 (repeatM
>                  ((p1 :+: Tempo (3%2) (p2 :+: enr :+: p2))
>                   :=: (p3 :+: qnr)) ))))

> t14d = let p1 = perc LowTom        en []
>            p2 = perc AcousticSnare hn []
>        in test (Instr "Drums"
>                   (  roll tn p1
>                  :+: p1
>                  :+: p1
>                  :+: Rest en
>                  :+: roll tn p1
>                  :+: p1
>                  :+: p1
>                  :+: Rest qn
>                  :+: roll tn p2
>                  :+: p1
>                  :+: p1  ))

-----------------------------------------------------------------------------

Tests of the MIDI interface.

Music into a MIDI file.

> tab m = do
>           outputMidiFile "test.mid" $ makeMidi (m, defCon, defUpm)

Music to a MidiFile datatype and back to Music.

> tad m = readMidi (testMidi m)

A MIDI file to a MidiFile datatype and back to a MIDI file.

> tcb file = do
>              x <- loadMidiFile file
>              outputMidiFile "test.mid" x

MIDI file to MidiFile datatype.

> tc file = do
>             x <- loadMidiFile file
>             print x

MIDI file to Music, a UserPatchMap, and a Context.

> tcd file = do
>              x <- loadMidiFile file
>              print $ fst3 $ readMidi x
>              print $ snd3 $ readMidi x
>              print $ thd3 $ readMidi x

A MIDI file to Music and back to a MIDI file.

> tcdab file = do
>              x <- loadMidiFile file
>              outputMidiFile "test.mid" $ makeMidi $ readMidi x

> getTracks (MidiFile _ _ trks) = trks
> 
> fst3 (a,b,c) = a
> snd3 (a,b,c) = b
> thd3 (a,b,c) = c

\end{verbatim} }
