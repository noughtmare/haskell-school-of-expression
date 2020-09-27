\section{Convenient Functions for Getting Started With Haskore}
\label{test-functions}

{\small
\begin{verbatim} 

> module TestHaskore where
> import Basics  
> import Performance
> import MidiFile
> import HaskToMidi 
> import GeneralMidi
> import OutputMidi
> import System.Process (system)
> import System.Info (os)

> ----------------------------------------------------------------------------
> -- Given a PMap, Context, UserPatchMap, and file name, we can 
> -- write a Music value into a midi file:
> ----------------------------------------------------------------------------
> mToMF :: PMap -> Context -> UserPatchMap -> String -> Music -> IO ()
> mToMF pmap c upm fn m =
>       let pf = perform pmap c m
>           mf = performToMidi pf upm
>       in outputMidiFile fn mf
>
> ----------------------------------------------------------------------------
> -- Convenient default values and test routines
> ----------------------------------------------------------------------------
> -- a default UserPatchMap
> -- Note: the PC sound card I'm using is limited to 9 instruments
> defUpm :: UserPatchMap
> defUpm = [("piano","Acoustic Grand Piano",1),
>           ("vibes","Vibraphone",2),
>           ("bass","Acoustic Bass",3),
>           ("flute","Flute",4),
>           ("sax","Tenor Sax",5),
>           ("guitar","Acoustic Guitar (steel)",6),
>           ("violin","Viola",7),
>           ("violins","String Ensemble 1",8),
>           ("drums","Acoustic Grand Piano",9)]  
>             -- the GM name for drums is unimportant, only channel 9
>
> -- a default PMap that makes everything into a fancyPlayer
> defPMap :: String -> Player
> defPMap pname =
>   MkPlayer pname nf pf sf
>   where MkPlayer _ nf pf sf = fancyPlayer
> 
> -- a default Context
> defCon :: Context
> defCon = Context { cTime   = 0,
>		     cPlayer = fancyPlayer,
>		     cInst   = "piano",
>		     cDur    = metro 120 qn,
>		     cKey    = 0,
>		     cVol    = 127 }
> 
> -- Using the defaults above, from a Music object, we can:
> -- a) generate a performance
> testPerf  :: Music -> Performance
> testPerf m = perform defPMap defCon m
> testPerfDur  :: Music -> (Performance, DurT)
> testPerfDur m = perf defPMap defCon m
> 
> -- b) generate a midifile datatype
> testMidi :: Music -> MidiFile
> testMidi m = performToMidi (testPerf m) defUpm
> 
> -- c) generate a midifile
> test     :: Music -> IO ()
> test     m = outputMidiFile "test.mid" (testMidi m)
> 

Alternatively, just run "play m", which will invoke the midi
player on your system using "play", defined below for Windows
and Linux (using timidity)

> playMidi :: String -> (Music -> IO ()) -> Music -> IO ()
> playMidi cmd tofile m =
>    do tofile m 
>       system (cmd ++ " test.mid")
>       return ()
>
> playOS = playMidi (if os == "mingw32"
>                    then "mplay32"
>                    else "timidity") 
>
> play = playOS test

A more general function in the tradition of testMidi, makeMidi also
takes a Context and a UserPatchMap.

> makeMidi :: (Music, Context, UserPatchMap) -> MidiFile
> makeMidi (m,c,upm) = performToMidi (perform defPMap c m) upm

> ----------------------------------------------------------------------------
> -- Some General Midi test functions (use with caution)
> ----------------------------------------------------------------------------
> -- a General Midi user patch map; i.e. one that maps GM instrument names
> -- to themselves, using a channel that is the patch number modulo 16.
> -- This is for use ONLY in the code that follows, o/w channel duplication
> -- is possible, which will screw things up in general.
> gmUpm :: UserPatchMap
> gmUpm = map (\(gmn,n) -> (gmn, gmn, mod n 16 + 1)) genMidiMap
> 
> -- Something to play each "instrument group" of 8 GM instruments;
> -- this function will play a C major arpeggio on each instrument.
> gmTest :: Int -> IO()
> gmTest i =  let gMM = take 8 (drop (i*8) genMidiMap)
>                 mu  = line (map simple gMM)
>                 simple (inm,_) = Instr inm cMajArp
>             in  mToMF defPMap defCon gmUpm "test.mid" mu

\end{verbatim} }
