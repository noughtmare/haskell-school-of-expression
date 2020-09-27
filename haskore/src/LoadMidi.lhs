\section{Loading MIDI Files}
\label{load}

The LoadMidi module loads and parses a MIDI File; it can convert it
into a MidiFile data type object or simply print out the contents of
the file.  

\begin{verbatim}

> module LoadMidi (loadMidiFile, showMidiFile)
>  where
> import MidiFile
> import Bitops (fromBytes, bshiftl, bTrunc, bSplitAt)
> import Data.Maybe (fromJust)
> import HaskoreUtils (unlinesS, rightS, concatS)
> import Control.Monad
> import System.IO

\end{verbatim}

The main load function.
\begin{verbatim}

> loadMidiFile :: String -> IO MidiFile
> loadMidiFile filename = 
>   do 
>     contents <- openBinaryFile filename ReadMode >>= hGetContents
>     case runP parseMidiFile (contents, (AtBeginning,0),-1) of
>       Just (mf,"",_,_) -> return mf
>       Just (mf,_ ,_,_) -> do {print "Garbage left over." ; return mf}
>       Nothing          -> do print "Error reading midi file: unfamiliar format or file corrupt."
>                              return emptyMidiFile
>
> emptyMidiFile = MidiFile 0 (Ticks 0) [[]]

\end{verbatim}

A MIDI file is made of ``chunks,'' each of which is either a ``header chunk''
or a ``track chunk.''  To be correct, it must consist of one header chunk
followed by any number of track chunks, but for robustness's sake we ignore
any non-header chunks that come before a header chunk.  The header tells us
the number of tracks to come, which is passed to {\tt getTracks}.
\begin{verbatim}

> parseMidiFile :: MidiReader MidiFile
> parseMidiFile = do
>                   chunk <- getChunk
>                   case chunk of
>                     Header (format, nTracks, div) ->
>                       do
>                         tracks <- getTracks nTracks
>                         return (MidiFile format div tracks)
>                     _                             -> parseMidiFile

\end{verbatim}

Parse a number of track chunks.  Like {\tt parseMidiFile}, if a chunk is not
a track chunk, it is just ignored.
\begin{verbatim}

> getTracks :: Int -> MidiReader [Track]
> getTracks 0 = return []
> getTracks n = do
>                 chunk <- getChunk
>                 tracks <- getTracks (n-1)
>                 case chunk of
>                   Track t -> return (t:tracks)
>                   _       -> return tracks

\end{verbatim}

Parse a chunk, whether a header chunk, a track chunk, or otherwise.
A chunk consists of a four-byte type code (a header is ``MThd''; a
track is ``MTrk''), four bytes for the size of the coming data, and
the data itself.
\begin{verbatim}

> getChunk :: MidiReader Chunk
> getChunk = do
>                ty                     <- getN 4
>                size                   <- get4
>                setSize size
>                case ty of
>                  "MThd"    -> do
>                                 h <- header
>                                 return (Header h)
>                  "MTrk"    -> do
>                                 t <- track
>                                 return (Track (undelta t))
>                  otherwise -> do
>                                 g <- getN size
>                                 return AlienChunk
> 
> data Chunk = Header (Int, Int, Division)
>            | Track Track
>            | AlienChunk
>   deriving Eq

\end{verbatim}

Parse a Header Chunk.  A header consists of a format (0, 1, or 2),
the number of track chunks to come, and the smallest time division
to be used in reading the rest of the file.
\begin{verbatim}

> header :: MidiReader (Int, Int, Division)
> header = do
>            format   <- get2
>            nTracks  <- get2
>            div      <- getDivision
>            return (format, nTracks, div)

\end{verbatim}

The division is implemented thus: the most significant bit is 0 if it's
in ticks per quarter note; 1 if it's an SMPTE value.
\begin{verbatim}

> getDivision :: MidiReader Division
> getDivision = do
>                 x <- get1
>                 y <- get1
>                 if x < 128
>                   then return (Ticks (x*256+y))
>                   else return (SMPTE (256-x) y)

\end{verbatim}

A track is a series of events.  Parse a track, stopping when the size
is zero.
\begin{verbatim}

> track :: MidiReader [MEvent]
> track = do
>           size <- readSize
>           case size of
>             0 -> return []
>             _ -> do
>                    e  <- fancyEvent
>                    es <- track
>                    return (e:es)

\end{verbatim}

Each event is preceded by the delta time: the time in ticks between the 
last event and the current event.  Parse a time and an event, ignoring
System Exclusive messages.
\begin{verbatim}

> fancyEvent :: MidiReader MEvent
> fancyEvent = do
>                      time <- getVar
>                      e    <- event
>                      case e of
>                        Midi midiEvent -> return (MidiEvent time midiEvent)
>                        Meta metaEvent -> return (MetaEvent time metaEvent)
>                        _              -> return (NoEvent)

\end{verbatim}

Parse an event.  Note that in the case of a regular Midi Event, the tag is
the status, and we read the first byte of data before we call {\tt midiEvent}.
In the case of a MidiEvent with running status, we find out the status from
the parser (it's been nice enough to keep track of it for us), and the tag
that we've already gotten is the first byte of data.
\begin{verbatim}

> event :: MidiReader MidiFileEvent
> event = do 
>           tag <- get1
>           case tag of
>             240       -> do
>                          size     <- getVar
>     	                   contents <- getN size
>     	                   return (SysexStart contents)
>             247       -> do
>                          size     <- getVar
>     	                   contents <- getN size
>     	                   return (SysexCont contents)
>   	      255       -> do
>	                   code <- get1
>     	                   size <- getVar
>                          e    <- metaEvent code size
>     	                   return (Meta e)
>   	      x | x>127 -> do
>                          firstData <- get1
>                          e <- midiEvent (decodeStatus tag) firstData
>     	       	           return (Midi e)
>             _         -> do                       -- running status
>                          s <- readME
>                          e <- midiEvent s tag
>                          return (Midi e)
> 
> data MidiFileEvent = SysexStart String         -- F0
>                    | SysexCont  String         -- F7
>                    | Midi       MidiEvent
>                    | Meta       MetaEvent
>                    deriving Show

\end{verbatim}

Simpler versions of {\tt fancyTrack} and {\tt fancyEvent}, used in
the Show functions.
\begin{verbatim}

> plainTrack :: MidiReader [(ElapsedTime, MidiFileEvent)]
> plainTrack = oneOrMore plainEvent
>
> plainEvent :: MidiReader (ElapsedTime, MidiFileEvent)
> plainEvent = do
>                time <- getVar
>                e    <- event
>                return (time,e)
> 
> data WhichMidiEvent = AtBeginning
>                     | ItsaNoteOff
>                     | ItsaNoteOn
>                     | ItsaPolyAfter
>                     | ItsaControl
>                     | ItsaProgChange
>                     | ItsaMonoAfter
>                     | ItsaPitchBend
>        deriving Show
> type Status = (WhichMidiEvent, Int)

\end{verbatim}

Find out the status (MidiEvent type and channel) given a byte of data.
\begin{verbatim}

> decodeStatus :: Int -> Status
> decodeStatus tag = (w, channel)
>   where w = case code of
>                 8  -> ItsaNoteOff
>                 9  -> ItsaNoteOn
>                 10 -> ItsaPolyAfter
>                 11 -> ItsaControl
>                 12 -> ItsaProgChange
>                 13 -> ItsaMonoAfter
>                 14 -> ItsaPitchBend
>                 _  -> error "invalid MidiEvent code"
>         (code, channel) = bSplitAt 4 tag

\end{verbatim}

Parse a MIDI Event.  Note that since getting the first byte is a little
complex (there are issues with running status), it has already been
handled for us by {\tt event}.
\begin{verbatim}

> midiEvent :: Status -> Int -> MidiReader MidiEvent
> midiEvent s@(wME, channel) firstData
>   = do
>       setME s
>       case wME of
>         ItsaNoteOff    -> do v <- get1
>                              return (NoteOff channel firstData v)
>         ItsaNoteOn     -> do v <- get1
>                              case v of
>                                0 -> return (NoteOff channel firstData 0)
>                                _ -> return (NoteOn  channel firstData v)
>         ItsaPolyAfter  -> do v <- get1
>                              return (PolyAfter channel firstData v)
>         ItsaControl    -> do v <- get1
>                              return (Control channel firstData v)
>         ItsaProgChange -> return (ProgChange channel firstData)
>         ItsaMonoAfter  -> return (MonoAfter channel firstData)
>         ItsaPitchBend  -> do msb <- get1
>                              return (PitchBend channel (firstData+256*msb))
>         AtBeginning    -> error "AtBeginning"

\end{verbatim}

Parse a MetaEvent.
\begin{verbatim}

> metaEvent :: Int -> Int -> MidiReader MetaEvent
> metaEvent   0 _    = do x <- get2;      return (SequenceNum x)
> metaEvent   1 size = do s <- getN size; return (TextEvent   s)
> metaEvent   2 size = do s <- getN size; return (Copyright   s)
> metaEvent   3 size = do s <- getN size; return (TrackName   s)
> metaEvent   4 size = do s <- getN size; return (InstrName   s)
> metaEvent   5 size = do s <- getN size; return (Lyric       s)
> metaEvent   6 size = do s <- getN size; return (Marker      s)
> metaEvent   7 size = do s <- getN size; return (CuePoint    s)
> 
> metaEvent  32 _    = do c <- get1;   return (MIDIPrefix  c)
> metaEvent  47 _    =                 return (EndOfTrack   )
> metaEvent  81 _    = do t <- get3;   return (SetTempo    t)
> 
> metaEvent  84 _    = do {hrs    <- get1 ; mins <- get1 ; secs <- get1;
>                          frames <- get1 ; bits <- get1 ;
>                          return (SMPTEOffset hrs mins secs frames bits)}
> 
> metaEvent  88 _    = do
>                        n <- get1
>                        d <- get1
>                        c <- get1
>                        b <- get1
>                        return (TimeSig n d c b)
> 
> metaEvent  89 _    = do
>                        sf <- get1
>                        mi <- get1
>                        return (KeySig (toKeyName sf) (toMode mi))
> 
> metaEvent 127 size = do
>                        contents <- getN size
>                        return (SequencerSpecific (map fromEnum contents))
> 
> metaEvent _   size = do {s <- getN size; return (Unknown s)}
> 
> toKeyName sf = [ KeyCf .. ] !! ((sf+7) `mod` 15)

\end{verbatim}

Convert a track using delta time to one using absolute time.
\begin{verbatim}

> undelta :: Track -> Track
> undelta trk = undelta' trk 0
>   where
>     undelta' :: Track -> Int -> Track
>     undelta' []                    _ = []
>     undelta' ((MidiEvent dt e):es) t = (MidiEvent (dt+t) e):undelta' es (dt+t)
>     undelta' ((MetaEvent dt e):es) t = (MetaEvent (dt+t) e):undelta' es (dt+t)
>     undelta' (_:es)                t = undelta' es t
>
> toMode :: Int -> Mode
> toMode 0 = Major
> toMode 1 = Minor

\end{verbatim}

{\tt getCh} gets a single character (a byte) from the input.
\begin{verbatim}

> getCh :: MidiReader Char
> getCh = do {sub1Size; tokenP myHead}
>           where  myHead ([]    ,_ ,_ ) = Nothing
>                  myHead ((c:cs),st,sz) = Just (c,cs,st,sz)

\end{verbatim}

{\tt getN n} returns n characters (bytes) from the input.
\begin{verbatim}

> getN :: Int -> MidiReader String
> getN 0 = return []
> getN n = do
>            a <- getCh
>            b <- (getN (n-1))
>            return (a:b)

\end{verbatim}

{\tt get1}, {\tt get2}, {\tt get3}, and {\tt get4} take 1-, 2-, 3-, or
4-byte numbers from the input (respectively), convert the base-256 data
into a single number, and return.
\begin{verbatim}

> get1 :: MidiReader Int
> get1 = do
>          c <- getCh
>          return (fromEnum c)
> 
> get2 :: MidiReader Int
> get2 = do
>          x1 <- get1
>          x2 <- get1
>          return (fromBytes [x1,x2])
> 
> get3 :: MidiReader Int
> get3 = do
>          x1 <- get1
>	   x2 <- get1
>	   x3 <- get1
>          return (fromBytes [x1,x2,x3])
> 
> get4 :: MidiReader Int
> get4 = do
>          x1 <- get1
>          x2 <- get1
>          x3 <- get1
>          x4 <- get1
>          return (fromBytes [x1,x2,x3,x4])

\end{verbatim}

{\em Variable-length quantities} are used often in MIDI notation.  They are
represented in the following way.  Each byte (containing 8 bits) uses the 7
least significant bits to store information.  The most significant bit is used
to signal whether or not more information is coming.  If it's 1, another byte
is coming.  If it's 0, that byte is the last one.  {\tt getVar} gets a
variable-length quantity from the input.
\begin{verbatim}

> getVar :: MidiReader Int
> getVar = getVarAux 0
>   where getVarAux n = do
>                         digit <- get1
>                         if digit < 128              -- if it's the last byte
>                           then return ((bshiftl 7 n) + digit)
>                           else getVarAux ((bshiftl 7 n) + (bTrunc 7 digit))

\end{verbatim}

Functions to show the decoded contents of a Midi file in an easy-to-read format.
\begin{verbatim}

> showMidiFile :: String -> IO ()
> showMidiFile file = do
>                       s <- openBinaryFile file ReadMode >>= hGetContents
>                       putStr (showChunks s)
> 
> showChunks :: String -> String
> showChunks mf = showMR chunks (unlinesS . map pp) (mf, (AtBeginning,0),-1) ""
>  where
>   pp :: (String, String, Status, Int) -> ShowS
>   pp ("MThd",contents,st,sz) =
>     showString "Header: " .
>     showMR header shows (contents,st,sz)
>   pp ("MTrk",contents,st,sz) =
>     showString "Track:\n" .
>     showMR plainTrack (unlinesS . map showTrackEvent) (contents,st,sz)
>   pp (ty,contents,_,_) =
>     showString "Chunk: " .
>     showString ty .
>     showString " " .
>     shows (map fromEnum contents) .
>     showString "\n"
> 
> showTrackEvent :: (ElapsedTime, MidiFileEvent) -> ShowS
> showTrackEvent (t,e) = 
>   rightS 10 (shows t) . showString " : " . showEvent e
> 
> showEvent :: MidiFileEvent -> ShowS
> showEvent (Midi e) = 
>   showString "MidiEvent " .
>   shows e
> showEvent (Meta e) = 
>   showString "MetaEvent " .
>   shows e
> showEvent (SysexStart s) = 
>   showString "SysexStart " . concatS (map (shows.fromEnum) s)
> showEvent (SysexCont s) =
>   showString "SysexCont "  . concatS (map (shows.fromEnum) s)
> 
> showMR :: MidiReader a -> (a->ShowS) -> (String, Status, Int) -> ShowS
> showMR m pp (s,st,sz) =
>   case runP m (s,st,sz) of
>   Nothing           -> showString "Parse failed: " . shows (map fromEnum s)
>   Just (a,[]  ,_,_) -> pp a
>   Just (a,junk,_,_) -> pp a . showString "Junk: " . shows (map fromEnum junk)

\end{verbatim}

These two functions, the plainChunk and chunks parsers, do not combine
directly into a single master parser.  Rather, they should be used to chop
parts of a midi file up into chunks of bytes which can be outputted separately.

Chop a Midi file into chunks returning:
\begin{itemize}
\item list of ``chunk-type''-contents-running status triples; and
\item leftover slop (should be empty in correctly formatted file)
\end{itemize}
\begin{verbatim}

> chunks :: MidiReader [(String, String, Status, Int)]
> chunks = zeroOrMore plainChunk
> 
> plainChunk :: MidiReader (String, String, Status, Int)
> plainChunk = do
>	         ty       <- getN 4       -- chunk type: header or track
>                size     <- get4         -- size of what's next
>                contents <- getN size    -- what's next
>                status   <- readME       -- running status
>                return (ty, contents, status, -1) -- Don't worry about size

\end{verbatim}

The following parser monad parses a Midi File.  As it parses, it keeps track of
these things:
\begin{itemize}
\item {\tt (w,c)} a.k.a. {\tt st}
          Running status.
          In MIDI, a shortcut is used for long strings of similar MIDI
          events: if a stream of consecutive events all have the same
          type and channel, the type and channel can be ommitted for all
          but the first event.  To implement this ``feature,''
          the parser must keep track of the type and channel of the most
          recent Midi Event.
\item {\tt sz}
          The size, in bytes, of what's left to parse, so that it knows
          when it's done.
\end{itemize}

\begin{verbatim}

> type MidiReader a = Parser String WhichMidiEvent Int Int a

> data Parser s w c sz a = P ((s,(w,c),sz) -> Maybe (a,s,(w,c),sz))

> unP :: Parser s w c sz a -> ((s,(w,c),sz) -> Maybe (a,s,(w,c),sz))
> unP (P a) = a
> 
> -- Access to state
> tokenP  :: ((s,(w,c),sz) -> Maybe (a,s,(w,c),sz)) -> Parser s w c sz a
> runP    :: Parser s w c sz a -> (s,(w,c),sz) -> Maybe (a,s,(w,c),sz)
> 
> tokenP get   = P $ get
> runP m (s,st,sz) = (unP m) (s,st,sz)
>
> instance Monad (Parser s w c sz) where
>   m >>= k  = P $ \ (s,st,sz) -> do
>                                      (a,s',st',sz') <- unP m (s,st,sz)
>                                      unP (k a) (s',st',sz')
>   m >>  k  = P $ \ (s,st,sz) -> do
>                                      (_,s',st',sz') <- unP m (s,st,sz)
>                                      unP k     (s',st',sz')
>   return a = P $ \ (s,st,sz) -> return (a,s,st,sz)
>
> setME  :: Status -> MidiReader ()
> setME st' = P $ \ (s,st,sz) -> return ((),s,st',sz)
>
> readME :: MidiReader Status
> readME  = P $ \ (s,st,sz) -> return (st,s,st,sz)
>
> setSize :: Int -> MidiReader ()
> setSize sz' = P $ \ (s,st,sz) -> return ((),s,st,sz')
> 
> sub1Size :: MidiReader ()
> sub1Size = P $ \ (s,st,sz) -> return ((),s,st,(sz-1))
>
> readSize :: MidiReader Int
> readSize = P $ \ (s,st,sz) -> return (sz,s,st,sz)
>
> -- instance MonadZero (Parser s w c sz) where
> 
> instance MonadPlus (Parser s w c sz) where
>   mzero = P $ \ (s,st,sz) -> mzero
>   p `mplus` q = P $ \ (s,st,sz) -> unP p (s,st,sz) `mplus` unP q (s,st,sz)
> 
> -- Wadler's force function
> force             :: Parser s w c sz a -> Parser s w c sz a
> force (P p)        = P $ \ (s,st,sz) -> let x = p (s,st,sz)
>                                         in Just (fromJust x)
>
> zeroOrMore        :: Parser s w c sz a -> Parser s w c sz [a]
> zeroOrMore p       = force (oneOrMore p `mplus` return [])
> 
> oneOrMore         :: Parser s w c sz a -> Parser s w c sz [a]
> oneOrMore p        = do {x <- p; xs <- zeroOrMore p; return (x:xs)}

\end{verbatim}
