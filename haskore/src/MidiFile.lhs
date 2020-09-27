\subsection{Midi-File Datatypes}

\begin{verbatim}

> module MidiFile(
>	 MidiFile(..), Division(..), Track, MFType, MEvent(..), ElapsedTime,
>	 MPitch, Velocity, ControlNum, PBRange, ProgNum, Pressure,
>	 MidiChannel, ControlVal,
>	 MidiEvent(..),
>	 MTempo, SMPTEHours, SMPTEMins, SMPTESecs, SMPTEFrames, SMPTEBits,
>	 MetaEvent(..),
>	 KeyName(..), Mode(..),
>        defST, defDurT
>	 ) where

> import Data.Ix

\end{verbatim}

\begin{verbatim}

The datatypes for Midi Files and Midi Events
------------------------------------------------------------------------

> data MidiFile = MidiFile MFType Division [Track] deriving (Show, Eq)
>
> data Division = Ticks Int | SMPTE Int Int
>      deriving (Show,Eq)
> 
> type Track  = [MEvent]
> type MFType = Int
> 
> data MEvent = MidiEvent ElapsedTime MidiEvent
>             | MetaEvent ElapsedTime MetaEvent
>             | NoEvent
>      deriving (Show,Eq)
> 
> type ElapsedTime  = Int
> 
> -- Midi Events
> 
> type MPitch      = Int
> type Velocity    = Int
> type ControlNum  = Int
> type PBRange     = Int
> type ProgNum     = Int
> type Pressure    = Int
> type MidiChannel = Int
> type ControlVal  = Int
> 
> data MidiEvent = NoteOff    MidiChannel MPitch Velocity
>                | NoteOn     MidiChannel MPitch Velocity
>                | PolyAfter  MidiChannel MPitch Pressure
>                | ProgChange MidiChannel ProgNum
>                | Control    MidiChannel ControlNum ControlVal
>                | PitchBend  MidiChannel PBRange
>                | MonoAfter  MidiChannel Pressure
>      deriving (Show, Eq)
> 
> -- Meta Events
> 
> type MTempo      = Int
> type SMPTEHours  = Int
> type SMPTEMins   = Int
> type SMPTESecs   = Int
> type SMPTEFrames = Int
> type SMPTEBits   = Int
> 
> data MetaEvent = SequenceNum Int
>                | TextEvent String
>                | Copyright String
>                | TrackName String
>                | InstrName String
>                | Lyric String
>                | Marker String
>                | CuePoint String
>                | MIDIPrefix MidiChannel
>                | EndOfTrack
>                | SetTempo MTempo
>                | SMPTEOffset SMPTEHours SMPTEMins SMPTESecs SMPTEFrames SMPTEBits
>                | TimeSig Int Int Int Int
>                | KeySig KeyName Mode
>                | SequencerSpecific [Int]
>                | Unknown String
>      deriving (Show, Eq)
> 

\end{verbatim}

The following enumerated type lists all the keys in order of their key
signatures from flats to sharps.  (Cf = 7 flats, Gf = 6 flats ... F =
1 flat, C = 0 flats/sharps, G = 1 sharp, ... Cs = 7 sharps.)  Useful
for transposition.  \begin{verbatim}

> data KeyName = KeyCf | KeyGf | KeyDf | KeyAf | KeyEf | KeyBf | KeyF
>              | KeyC | KeyG | KeyD | KeyA | KeyE | KeyB | KeyFs | KeyCs
>              deriving (Eq, Ord, Ix, Enum, Show)

\end{verbatim}

The Key Signature specifies a mode, either major or minor.
\begin{verbatim}

> data Mode = Major | Minor
>             deriving (Show, Eq)

\end{verbatim}

Default duration of a whole note, in seconds; and the default SetTempo
value, in microseconds per quarter note.  Both express the default of
120 beats per minute.
\begin{verbatim}

> defDurT = 2 :: Float
> defST = truncate (1000000 / defDurT) :: Int

\end{verbatim}
