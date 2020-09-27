\section{Outputting MIDI Files}
\label{output}

The functions in this module allow {\tt MidiFile}s to be made into
Standard MIDI files (*.mid) that can be read and played by music
programs such as Cakewalk.

\begin{verbatim}

> module OutputMidi (outputMidiFile, midiFileToString) where
> import MidiFile
> import Monads (Output, runO, outO)
> import Bitops (bSplitAt, someBytes)
> import Data.Ix
> import System.IO

\end{verbatim}

{\tt OutputMidiFile} is the main function for writing {\tt MidiFile}
values to an actual file; its first argument is the filename:
\begin{verbatim}

> outputMidiFile :: String -> MidiFile -> IO ()
> outputMidiFile fn mf = do
>       putStrLn $ "writing to " ++ fn
>       h <- openBinaryFile fn WriteMode
>       hPutStr h (midiFileToString mf)
>       hClose h
>       putStrLn $ "finish writing to " ++ fn

\end{verbatim}

\begin{exercise} Take as many examples as you like from the previous
sections, create one or more {\tt UserPatchMaps}, write the examples
to a file, and play them using a conventional Midi player.
\end{exercise}
Appendix \ref{test-functions} defines some functions which should make
the above exercise easier.  Appendices \ref{examples}, \ref{chick},
and \ref{self-similar} contain more extensive examples.

Midi files are first converted to a monadic string computation using
the function {\tt outMF}, and then "executed" using
{\tt runM :: MidiWriter a -> String}.

\begin{verbatim} 

> midiFileToString :: MidiFile -> String
> midiFileToString = runM . outMF
>
> outMF :: MidiFile -> MidiWriter ()
> outMF (MidiFile mft divisn trks) =
>   do
>     outChunk "MThd" (do
>                        out 2 mft                   -- format (type 0, 1 or 2)
>                        out 2 (length trks)         -- length of tracks to come
>                        outputDivision divisn)      -- time unit
>     outputTracks trks
> 
> outputDivision :: Division -> MidiWriter ()
> outputDivision (Ticks nticks)      = out 2 nticks
> outputDivision (SMPTE mode nticks) = do
>                                        out 1 (256-mode)
>                                        out 1 nticks
> 
> outputTracks :: [Track] -> MidiWriter ()
> outputTracks trks = mapM_ outputTrack trks
> 
> outputTrack :: Track -> MidiWriter ()
> outputTrack trk = outChunk "MTrk" (mapM_ outputEvent (delta trk))

\end{verbatim}

{\tt delta} converts a track using absolute time to one using delta time, adding
EndOfTrack if not already there.
\begin{verbatim}

> delta :: Track -> Track
> delta [] = []
> delta trk | notEOT (last trk) = trk' ++ [MetaEvent 0 EndOfTrack]
>           | otherwise         = trk'
>  where
>   (t,trk') = mscanl delta' 0 trk
>   delta' :: Int ->       -- current time
>             MEvent ->    -- event
>             (Int,        -- new time
>              MEvent)     -- event
>   delta' t (MidiEvent dt e) = (dt  , MidiEvent (dt-t) e)
>   delta' t (MetaEvent dt e) = (dt  , MetaEvent (dt-t) e)
>   notEOT (MetaEvent _ EndOfTrack) = False
>   notEOT _                        = True

\end{verbatim}

The following functions encode various {\tt MidiFile} elements into
the raw data of a standard MIDI file.
\begin{verbatim}

> outputEvent :: MEvent -> MidiWriter ()
> outputEvent (MidiEvent dt mevent) = do
>                                       outVar dt
>                                       outputMidiEvent mevent
> outputEvent (MetaEvent dt mevent) = do
>                                       outVar dt
>                                       outputMetaEvent mevent
> outputEvent _                     = outStr ""
>
> outputMidiEvent :: MidiEvent -> MidiWriter ()
> outputMidiEvent (NoteOff    c p v)   = outChan 128 c [p,v]
> outputMidiEvent (NoteOn     c p v)   = outChan 144 c [p,v]
> outputMidiEvent (PolyAfter  c p pr)  = outChan 160 c [p,pr]
> outputMidiEvent (Control    c cn cv) = outChan 176 c [cn,cv]
> outputMidiEvent (ProgChange c pn)    = outChan 192 c [pn]
> outputMidiEvent (MonoAfter  c pr)    = outChan 208 c [pr]
> outputMidiEvent (PitchBend  c pb)    = outChan 224 c [lo,hi] -- small-endian!!
>  where (hi,lo) = bSplitAt 8 pb
>
> -- output a channel event
> outChan :: Int -> MidiChannel -> [Int] -> MidiWriter ()
> outChan code chan bytes = do
>                             out 1 (code+chan)
>                             mapM_ (out 1) bytes
> 
> 
> outMeta    :: Int -> [Int] -> MidiWriter ()
> outMeta code bytes = do
>                        out 1 255
>                        out 1 code
>                        outVar (length bytes)
>                        outList bytes
> 
> outMetaStr :: Int -> String -> MidiWriter ()
> outMetaStr code bytes = do
>                           out 1 255
>                           out 1 code
>                           outVar (length bytes)
>                           outStr bytes
> 
> -- As with outChunk, there are other ways to do this - but
> -- it's not obvious which is best or if performance is a big issue.
> outMetaMW :: Int -> MidiWriter a -> MidiWriter a
> outMetaMW code m = do
>                      out 1 255              
>                      out 1 code             
>                      outVar (mLength m)    
>                      m
> 
> outputMetaEvent :: MetaEvent -> MidiWriter ()
> outputMetaEvent (SequenceNum num) = outMetaMW   0 (out 2 num)
> outputMetaEvent (TextEvent s)     = outMetaStr  1 s
> outputMetaEvent (Copyright s)     = outMetaStr  2 s
> outputMetaEvent (TrackName s)     = outMetaStr  3 s
> outputMetaEvent (InstrName s)     = outMetaStr  4 s
> outputMetaEvent (Lyric s)         = outMetaStr  5 s
> outputMetaEvent (Marker s)        = outMetaStr  6 s
> outputMetaEvent (CuePoint s)      = outMetaStr  7 s
> outputMetaEvent (MIDIPrefix c)    = outMeta    32 [c]
> outputMetaEvent EndOfTrack        = outMeta    47 []
> 
> outputMetaEvent (SetTempo tp)     = outMetaMW  81 (out 3 tp)
> outputMetaEvent (SMPTEOffset hr mn se fr ff) 
>                                   = outMeta    84 [hr,mn,se,fr,ff]
> outputMetaEvent (TimeSig n d c b) = outMeta    88 [n,d,c,b]
> outputMetaEvent (KeySig sf mi)    = outMeta    89 [convert sf, fromMode mi]
>                                       where k = index (KeyCf,KeyCs) sf - 7
>                                             convert sf = if (k >= 0) then k
>                                                                      else 255+k
> outputMetaEvent (SequencerSpecific codes) 
>                                   = outMeta    127 codes
> outputMetaEvent (Unknown s)       = outMetaStr 21 s

\end{verbatim}

The midiwriter accumulates a String.
For all the usual reasons, the String is represented by ShowS.
\begin{verbatim}
 
> type MidiWriter a = Output Char a
> 
> out :: Int -> Int -> MidiWriter ()
> outVar :: Int -> MidiWriter ()
> outList :: [Int] -> MidiWriter ()
> outStr  :: String -> MidiWriter ()
> 
> runM :: MidiWriter a -> String
> runM m = snd (runO m)
> 
> mLength  :: MidiWriter a -> Int
> mLength m = length (runM m)
> 
> out 1 x = outO [toEnum x]
> out a x = mapM_ (out 1) (someBytes a x)
> 
> outStr cs = outO cs
>
> outList xs = outStr (map toEnum xs)

\end{verbatim}

Numbers of variable size are represented by sequences of 7-bit blocks
tagged (in the top bit) with a bit indicating:
(1) that more data follows; or
(0) that this is the last block.
\begin{verbatim}
 
> outVar n = do
>              outVarAux leftover
>              out 1 data7
>            where (leftover, data7) = bSplitAt 7 n
>                  outVarAux 0 = return ()
>                  outVarAux x = do
>                                  outVarAux leftover'
>                                  out 1 (128+data7')  --make signal bit 1
>                                where (leftover',data7') = bSplitAt 7 x
>
> fromMode :: Mode -> Int
> fromMode Major = 0
> fromMode Minor = 1
>
> -- Note: here I've chosen to compute the track twice 
> -- rather than store it.  Other options are worth exploring.
>
> outChunk :: String -> MidiWriter a -> MidiWriter a
> outChunk tag m | length tag == 4 = do
>                                      outStr tag
>                                      out 4 (mLength m)
>                                      m

\end{verbatim}

Mapping scan (used in function delta):
\begin{verbatim}
          x                 xs
          |                 |
          V                 V
        +---+         +----------+
  l ->  | f | -> m -> | mscanl f | -> r
        +---+         +----------+
          |                 |
          V                 V
          y                 ys

> mscanl :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
> mscanl f l [] = (l,[])
> mscanl f l (x:xs) = let (m, y ) = f l x
>                         (r, ys) = mscanl f m xs
>                     in (r, y:ys)

\end{verbatim} 
