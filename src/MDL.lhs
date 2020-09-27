This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module MDL where
>
> import Music
> import Perform
> import Haskore ( MidiFile(..), MidiChannel, ProgNum, MEvent,
>                  MFType, Velocity, MEvent(..), MidiEvent(..),
>                  MetaEvent(..), Division(..), MTempo,
>                  outputMidiFile )
> import List ( partition )
> import System ( system )

< data MidiFile = MidiFile MFType Division [Track] 
<      deriving (Show, Eq)
<
< type MFType = Int
< type Track  = [MEvent]
< 
< data Division = Ticks Int | SMPTE Int Int
<      deriving (Show,Eq)
< 
< data MEvent = MidiEvent ElapsedTime MidiEvent
<             | MetaEvent ElapsedTime MetaEvent
<             | NoEvent
<      deriving (Show,Eq)
< 
< type ElapsedTime  = Int

< -- Midi Events
< data MidiEvent = NoteOff    MidiChannel MPitch Velocity
<                | NoteOn     MidiChannel MPitch Velocity
<                | ProgChange MidiChannel ProgNum
<                | ...
<      deriving (Show, Eq)
< type MPitch      = Int
< type Velocity    = Int
< type ProgNum     = Int
< type MidiChannel = Int

< -- Meta Events
< data MetaEvent = SetTempo MTempo
<                | ...
<      deriving (Show, Eq)
< type MTempo      = Int

> performToMidi :: Performance -> MidiFile
> performToMidi pf =
>   MidiFile mfType (Ticks division)
>                   (map performToMEvs (splitByInst pf))

> mfType   = 1  :: Int
> division = 96 :: Int

> splitByInst :: Performance -> [(MidiChannel,ProgNum,Performance)]

> splitByInst p
>   = aux 0 p where
>       aux n [] = []
>       aux n pf = let i         = eInst (head pf)
>                      (pf1,pf2) = partition (\e -> eInst e == i) pf
>                      n'        = if n==8 then 10 else n+1
>                  in if i==Percussion
>                     then (9, 0, pf1) : aux n pf2
>                     else if n>15 
>                          then error "No more than 16 instruments allowed"
>                          else (n, fromEnum i, pf1) : aux n' pf2

< partition :: (a -> Bool) -> [a] -> ([a],[a])
< partition p xs =  
<   foldr select ([],[]) xs
<         where select x (ts,fs) | p x       = (x:ts,fs)
<                                | otherwise = (ts, x:fs)

> performToMEvs :: (MidiChannel,ProgNum,Performance) -> [MEvent]

> performToMEvs (ch,pn,perf)
>   = let setupInst   = MidiEvent 0 (ProgChange ch pn)
>         setTempo    = MetaEvent 0 (SetTempo tempo)
>         loop []     = []
>         loop (e:es) = let (mev1,mev2) = mkMEvents ch e
>                       in  mev1 : insertMEvent mev2 (loop es)
>     in  setupInst : setTempo : loop perf

> tempo = 500000 :: Int -- number of microseconds in one beat

> mkMEvents :: MidiChannel -> Event -> (MEvent,MEvent)

> mkMEvents mChan (Event { eTime = t, ePitch = p, eDur = d })
>                   = (MidiEvent (toDelta t)     (NoteOn  mChan p 127),
>                      MidiEvent (toDelta (t+d)) (NoteOff mChan p 127) )
>
> toDelta t = round (t * 4.0 * intToFloat division)

> insertMEvent :: MEvent -> [MEvent] -> [MEvent]

> insertMEvent ev1  []         
>   = [ev1]
> insertMEvent ev1@(MidiEvent t1 _) evs@(ev2@(MidiEvent t2 _):evs') 
>   = if t1 <= t2 then ev1 : evs
>                 else ev2 : insertMEvent ev1 evs'

< outputMidiFile :: String -> MidiFile -> IO ()

> test :: Music -> IO ()
> test m = outputMidiFile "test.mid" 
>            (performToMidi (perform defCon m))
>
> defCon :: Context
> defCon = Context { cTime   = 0,
>		     cInst   = AcousticGrandPiano,
>		     cDur    = metro 120 qn,
>		     cKey    = 0 }

> testWin95, testNT, testLinux :: Music -> IO ()

> testWin95 m = do test m
>                  system "mplayer test.mid"
>                  return ()
>
> testNT    m = do test m
>                  system "mplay32 test.mid"
>                  return ()
>
> testLinux m = do test m
>                  system "playmidi -rf test.mid"
>                  return ()

| testNT funkGroove

