\begin{verbatim}

> module Drums (module Basics, module Trill, module Drums) where
> import Basics
> import Trill
>
> data DrumSound =
> 	  AcousticBassDrum  -- Midi Key 35
>	| BassDrum1	    -- Midi Key 36
>	| SideStick         -- ...
>	| AcousticSnare
>	| HandClap
>	| ElectricSnare
>	| LowFloorTom
>	| ClosedHiHat
>	| HighFloorTom
>	| PedalHiHat
>	| LowTom
>	| OpenHiHat
>	| LowMidTom
>	| HiMidTom
>	| CrashCymbal1
>	| HighTom
>	| RideCymbal1
>	| ChineseCymbal
>	| RideBell
>	| Tambourine
>	| SplashCymbal
>	| Cowbell
>	| CrashCymbal2
>	| Vibraslap
>	| RideCymbal2
>	| HiBongo
>	| LowBongo
>	| MuteHiConga
>	| OpenHiConga
>	| LowConga
>	| HighTimbale
>	| LowTimbale
>	| HighAgogo
>	| LowAgogo
>	| Cabasa
>	| Maracas
>	| ShortWhistle
>	| LongWhistle
>	| ShortGuiro
>	| LongGuiro
>	| Claves
>	| HiWoodBlock
>	| LowWoodBlock
>	| MuteCuica
>	| OpenCuica         -- ...
>	| MuteTriangle      -- Midi Key 80
>	| OpenTriangle      -- Midi Key 81
>    deriving (Show,Eq,Ord,Ix,Enum)
>
> drum :: DrumSound -> Dur -> [NoteAttribute] -> Music
> drum ds dur na = Note (pitch (fromEnum ds + 35)) dur na
>
> data DrumElement = N         Dur [NoteAttribute] -- note
>                  | R         Dur                 -- rest
>                  | Roll  Dur Dur [NoteAttribute] -- roll w/duration
>                  | Rolln Int Dur [NoteAttribute] -- roll w/number of strokes
>
> drumLine :: DrumSound -> [DrumElement] -> Music
> drumLine dsnd l = Instr "Drums" (dlAux dsnd l) where
>	dlAux dsnd []                        = Rest 0
>	dlAux dsnd (N dur na :xs)            = drum dsnd dur na
>                                               :+: dlAux dsnd xs
>	dlAux dsnd (R dur :xs)               = Rest dur
>                                               :+: dlAux dsnd xs
>	dlAux dsnd (Roll sDur dur na :xs)    = roll sDur (drum dsnd dur na)
>                                               :+: dlAux dsnd xs
>	dlAux dsnd (Rolln nTimes dur na :xs) = rolln nTimes (drum dsnd dur na)
>                                               :+: dlAux dsnd xs

\end{verbatim}
