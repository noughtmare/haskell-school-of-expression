\section{Haskore Support for CSound}
\label{csound-sect}

\begin{verbatim} 

> module CSound (module Performance, module CSound)
>        where
>
> import Performance
> import System.IO
> import Data.List (find, nub)

\end{verbatim} 

[Note: if this module is loaded into Hugs98, the following error
message may result:
\begin{verbatim} 
    Reading file "CSound.lhs":
    ERROR "CSound.lhs" (line 707):
    *** Cannot derive Eq OrcExp after 40 iterations.
    *** This may indicate that the problem is undecidable.  However,
    *** you may still try to increase the cutoff limit using the -c
    *** option and then try again.  (The current setting is -c40)
\end{verbatim} 
This is apparently due to the size of the {\tt OrcExp} data type.  For
correct operation, start Hugs with a larger cutoff limit, such as {\tt
-c1000}.]

CSound is a software synthesizer that allows its user to create a
virtually unlimited number of sounds and instruments.  It is extremely
portable because it is written entirely in C.  Its strength lies
mainly in the fact that all computations are performed in software, so
it is not reliant on sophisticated musical hardware.  The output of a
CSound computation is a file representing the signal which can be
played by an independent application, so there is no hard upper limit
on computation time.  This is important because many sophisticated
signals take much longer to compute than to play.  The purpose of this
module is to create an interface between Haskore and CSound in order
to give the Haskore user access to all the powerful features of a
software sound synthesizer.

CSound takes as input two plain text files: a {\em score} (.sco) file
and an {\em orchestra} (.orc) file.  The score file is similar to a
Midi file, and the orchestra file defines one or more {\em
instruments} that are referenced from the score file (the orchestra
file can thus be thought of as the software equivalent of Midi
hardware).  The CSound program takes these two files as input, and
produces a {\em sound file} as output, usually in {\tt .wav} format.
Sound files are generally much larger than Midi files, since they
describe the actual sound to be generated, represented as a sequence
of values (typically 44,100 of them for each second of music), which
are converted directly into voltages that drive the audio speakers.
Sound files can be played by any standard media player found on
conventional PC's.

Each of these files is described in detail in the following sections.

\subsection{The Score File}
\label{score-file-sect}

We will represent a score file as a sequence of {\em score
statements}: 
\begin{verbatim} 

> type Score = [ScoreStmt]

\end{verbatim} 
The {\tt ScoreStmt} data type is designed to simulate CSound's three
kinds of score statements:
\begin{enumerate} 
\item A {\em tempo} statement, which sets the tempo.  In the absence
of a tempo statement, the tempo defaults to 60 beats per minute.

\item A {\em note event}, which defines the start time, pitch,
duration (in beats), volume (in decibels), and instrument to play a
note (and is thus more like a Haskore {\tt Event} than a Midi event,
thus making the conversion to CSound easier than to Midi, as we shall
see later).  Each note event also contains a number of optional
arguments called {\em p-fields}, which determine other properties of
the note, and whose interpretation depends on the instrument that
plays the note.  This will be discussed further in a later section.

\item {\em Function table} definitions.  A function table is used by
instruments to produce audio signals.  For example, sequencing through
a table containing a perfect sine wave will produce a very pure tone,
while a table containing an elaborate polynomial will produce a
complex sound with many overtones.  The tables can also be used to
produce control signals that modify other signals.  Perhaps the
simplest example of this is a tremolo or vibrato effect, but more
complex sound effects, and FM (frequency modulation) synthesis in
general, is possible.
\end{enumerate}

\begin{verbatim} 

> data ScoreStmt = CSTempo Bpm
>                | CSNote Inst StartTime Duration Pch Vlm [Pfield]        
>                | CSTable Tbl CreatTime TblSize Normalize GenRoutine
>      deriving Show
>
> type Bpm       = Int
> type Inst      = Int
> type StartTime = Float 
> type Duration  = Float
> type Pch       = Float
> type Vlm       = Float
> type Pfield    = Float
> type Tbl       = Int
> type CreatTime = Float
> type TblSize   = Int
> type Normalize = Bool
	
\end{verbatim} 

This is all rather straightforward, except for function table
generation, which requires further explanation.

\subsubsection{Function Tables}
\label{function-table-sect}

Each function table must have a unique integer ID ({\tt Tbl}),
creation time (usually 0), size (which must be a power of 2), and a
{\tt Normalize} flag.  Most tables in CSound are normalized, i.e.\
rescaled to a maximum absolute value of 1.  The normalization process
can be skipped by setting the {\tt Normalize} flag to {\tt False}.
Such a table may be desirable to generate a control or modifying
signal, but is not very useful for audio signal generation.

Tables are simply arrays of floating point values.  The values stored
in the table are calculated by one of CSound's predefined {\em generating
routines}, represented by the type {\tt GenRoutine}:
\begin{verbatim} 

> data GenRoutine = GenRoutine GenNum [GenArg]
>                 | SoundFile SFName SkipTime ChanNum
>      deriving Show
>
> type SFName   = String
> type SkipTime = Float
> type ChanNum  = Float
> type GenNum   = Int
> type GenArg   = Float 

\end{verbatim} 
{\tt GenRoutine n args} refers to CSound's generating routine $n$ (an
integer), called with floating point arguments {\tt args}.  There is
only one generating routine (called GEN01) in CSound that takes an
argument type other than floating point, and thus we represent this
using the special constructor {\tt SoundFile}, whose functionality
will be described shortly.
	
Knowing which of CSound's generating routines to use and with what
arguments can be a daunting task.  The newest version of CSound
(version 4.01) provides 23 different generating routines, and each one
of them assigns special meanings to its arguments.  To avoid having to
reference routines using integer ids, the following functions are
defined for the most often-used generating routines.  A brief
discussion of each routine is also included.  For a full description
of these and other routines, refer to the CSound manual or consult the
following webpage: {\tt
http://www.leeds.ac.uk/music/Man/Csound/Function/GENS.html}.  The user
familiar with CSound is free to write helper functions like the ones
below to capture other generating routines.

\paragraph*{GEN01.} Transfers data from a soundfile into a function
table.  Recall that the size of the function table in CSound must be a
power of two.  If the soundfile is larger than the table size, reading
stops when the table is full; if it is smaller, then the table is
padded with zeros.  One exception is allowed: if the file is of type
AIFF and the table size is set to zero, the size of the function table
is allocated dynamically as the number of points in the soundfile.
The table is then unusable by normal oscillators, but can be used by a
special {\tt SampOsc} constructor (discussed in Section
\ref{orchestra-file-sect}).  The first argument passed to the GEN01
subroutine is a string containing the name of the source file.  The
second argument is skip time, which is the number of seconds into the
file that the reading begins.  Finally there is an argument for the
channel number, with 0 meaning read all channels.  GEN01 is
represented in Haskore as {\tt SoundFile SFName SkipTime ChanNum}, as
discussed earlier.  To make the use of {\tt SoundFile} consistent with
the use of other functions to be described shortly, we define a simple
equivalent:
\begin{verbatim} 

> soundFile :: SFName -> SkipTime -> ChanNum -> GenRoutine
> soundFile = SoundFile

\end{verbatim} 

\paragraph*{GEN02.} Transfers data from its argument fields directly
into the function table.  We represent its functionality as follows:
\begin{verbatim} 

> tableValues :: [GenArg] -> GenRoutine 
> tableValues gas = GenRoutine 2 gas

\end{verbatim} 

\paragraph*{GEN03.} Fills the table by evaluating a polynomial over a
specified interval and with given coefficients.  For example, calling
GEN03 with an interval of $(-1,1)$ and coefficients 5, 4, 3, 2, 0, 1
will generate values of the function $5+4x+3x^2+2x^3+x^5$ over the
interval $-1$ to $1$.  The number of values generated is equal to the
size of the table.  Let's express this by the following function:
\begin{verbatim} 

> polynomial :: Interval -> Coefficients -> GenRoutine
> polynomial (x1,x2) cfs = GenRoutine 3 (x1:x2:cfs)
>
> type Interval     = (Float, Float)
> type Coefficients = [Float]

\end{verbatim} 

\paragraph*{GEN05.} Constructs a table from segments of exponential
curves.  The first argument is the starting point.  The meaning of the
subsequent arguments alternates between the length of a segment in
samples, and the endpoint of the segment.  The endpoint of one segment
is the starting point of the next.  The sum of all the segment lengths
normally equals the size of the table: if it is less the table is
padded with zeros, if it is more, only the first {\tt TblSize}
locations will be stored in the table.

\begin{verbatim} 

> exponential1 :: StartPt -> [(SegLength, EndPt)] -> GenRoutine
> exponential1 sp xs = GenRoutine 5 (sp : flattenTuples2 xs)
>
> type StartPt   = Float
> type SegLength = Float
> type EndPt     = Float

\end{verbatim} 
{\tt flattenTuples2} flattens a list of pairs into a list.  Similarly,
{\tt flattenTuples3} flattens a list of 3-tuples into a list, and so
on.
\begin{verbatim} 

> flattenTuples2 :: [(a,a)]     -> [a]
> flattenTuples3 :: [(a,a,a)]   -> [a]
> flattenTuples4 :: [(a,a,a,a)] -> [a]
>
> flattenTuples2 []           = []
> flattenTuples2 ((x,y) : xs) = x : y : flattenTuples2 xs
>
> flattenTuples3 []             = []
> flattenTuples3 ((x,y,z) : xs) = x : y : z : flattenTuples3 xs
> 
> flattenTuples4 []                  = []
> flattenTuples4 ((x, y, z, w) : xs) = x : y : z : w : flattenTuples4 xs

\end{verbatim} 

\paragraph*{GEN25.} Similar to GEN05 in that it produces segments of
exponential curves, but instead of representing the lengths of
segments and their endpoints, its arguments represent $(x,y)$
coordinates in the table, and the subroutine produces curves between
successive locations.  The $x$-coordinates must be in increasing
order.

\begin{verbatim} 

> exponential2 :: [Point] -> GenRoutine
> exponential2 pts = GenRoutine 25 (flattenTuples2 pts)
>
> type Point = (Float,Float)

\end{verbatim} 

\paragraph*{GEN06.} Generates a table from segments of cubic
polynomial functions, spanning three points at a time.  We define a
function {\tt cubic} with two arguments: a starting position and a
list of segment length (in number of samples) and segment endpoint
pairs.  The endpoint of one segment is the starting point of the next.
The meaning of the segment endpoint alternates between a local
minimum/maximum and point of inflexion.  Whether a point is a maximum
or a minimum is determined by its relation to the next point of
inflexion.  Also note that for two successive minima or maxima, the
inflexion points will be jagged, whereas for alternating maxima and
minima, they will be smooth.  The slope of the two segments is
independent at the point of inflection and will likely vary.  The
starting point is a local minimum or maximum (if the following point
is greater than the starting point, then the starting point is a
minimum, otherwise it is a maximum).  The first pair of numbers will
in essence indicate the position of the first inflexion point in
$(x,y)$ coordinates.  The folowing pair will determine the next local
minimum/maximum, followed by the second point of inflexion, etc.
\begin{verbatim} 

> cubic ::  StartPt -> [(SegLength, EndPt)] -> GenRoutine
> cubic sp pts = GenRoutine 6 (sp : flattenTuples2 pts)

\end{verbatim} 

\paragraph*{GEN07.} Similar to GEN05, except that it generates
straight lines instead of exponential curve segments.  All other
issues discussed about GEN05 also apply to GEN07.  We represent it as:
\begin{verbatim} 

> lineSeg1 :: StartPt -> [(SegLength, EndPt)] -> GenRoutine
> lineSeg1 sp pts = GenRoutine 7 (sp : flattenTuples2 pts)

\end{verbatim} 

\paragraph*{GEN27.} As with GEN05 and GEN25, produces straight line
segments between points whose locations are given as $(x,y)$
coordinates, rather than a list of segment length, endpoint pairs.
\begin{verbatim} 

> lineSeg2 :: [Point] -> GenRoutine
> lineSeg2 pts = GenRoutine 27 (flattenTuples2 pts)

\end{verbatim} 

\paragraph*{GEN08.} Produces a smooth piecewise cubic spline curve
through the specified points.  Neighboring segments have the same
slope at the common points, and it is that of a parabola through that
point and its two neighbors.  The slope is zero at the ends.
\begin{verbatim} 

> cubicSpline :: StartPt -> [(SegLength, EndPt)] -> GenRoutine
> cubicSpline sp pts = GenRoutine 8 (sp : flattenTuples2 pts)

\end{verbatim} 

\paragraph*{GEN10.} Produces a composite sinusoid.  It takes a list of
relative strengths of harmonic partials 1, 2, 3, etc.  Partials not
required should be given strength of zero.
\begin{verbatim} 

> compSine1 :: [PStrength] -> GenRoutine
> compSine1 pss = GenRoutine 10 pss
>
> type PStrength = Float

\end{verbatim} 

\paragraph*{GEN09.} Also produces a composite sinusoid, but requires
three arguments to specify each contributing partial.  The arguments
specify the partial number, which doesn't have to be an integer (i.e.\
inharmonic partials are allowed), the relative partial strength, and
the initial phase offset of each partial, expressed in degrees.
\begin{verbatim} 

> compSine2 :: [(PNum, PStrength, PhaseOffset)] -> GenRoutine
> compSine2 args = GenRoutine 9 (flattenTuples3 args)
>
> type PNum = Float
> type PhaseOffset = Float

\end{verbatim} 

\paragraph*{GEN19.} Provides all of the functionality of GEN09, but in
addition a DC offset must be specified for each partial.  The DC
offset is a vertical displacement, so that a value of 2 will lift a
2-strength partial from range $[-2,2]$ to range $[0,4]$ before further
scaling.
\begin{verbatim} 

> compSine3 :: [(PNum, PStrength, PhaseOffset, DCOffset)] -> GenRoutine
> compSine3 args = GenRoutine 19 (flattenTuples4 args)
>
> type DCOffset = Float

\end{verbatim} 

\paragraph*{GEN11.} Produces an additive set of harmonic cosine
partials, similar to GEN10.  We will represent it by a function that
takes three arguments: the number of harmonics present, the lowest
harmonic present, and a multiplier in an exponential series of
harmonics amplitudes (if the $x$'th harmonic has strength coefficient
of $A$, then the $(x+n)$'th harmonic will have a strength of
$A*(r^n)$, where $r$ is the multiplier).
\begin{verbatim} 

> cosineHarms :: NHarms -> LowestHarm -> Mult -> GenRoutine
> cosineHarms n l m = GenRoutine 11 [float n, float l, m]
>
> type NHarms = Int
> type LowestHarm = Int
> type Mult = Float

\end{verbatim} 

\paragraph*{GEN21.} Produces tables having selected random distributions.
\begin{verbatim} 

> randomTable :: RandDist -> GenRoutine
> randomTable rd = GenRoutine 21 [float rd]
>
> type RandDist = Int
>
> uniform, linear, triangular, expon, 
>   biexpon, gaussian, cauchy, posCauchy :: Int
> uniform    = 1
> linear     = 2
> triangular = 3
> expon      = 4
> biexpon    = 5
> gaussian   = 6
> cauchy     = 7
> posCauchy  = 8

\end{verbatim} 

\paragraph*{Common Tables}

For convenience, here are some common function tables, which take as
argument the identifier integer:
\begin{verbatim} 

> simpleSine, square, sawtooth, triangle, whiteNoise :: Tbl -> ScoreStmt
>
> simpleSine n = CSTable n 0 8192 True
>                       (compSine1 [1])
> square     n = CSTable n 0 1024 True
>                       (lineSeg1 1 [(256, 1), (0, -1), (512, -1), (0, 1), (256, 1)])
> sawtooth   n = CSTable n 0 1024 True
>                       (lineSeg1 0 [(512, 1), (0, -1), (512, 0)])
> triangle   n = CSTable n 0 1024 True
>                       (lineSeg1 0 [(256, 1), (512, -1), (256, 0)])
> whiteNoise n = CSTable n 0 1024 True
>                       (randomTable uniform) 

\end{verbatim} 
The following function for a composite sine has an extra argument, a
list of harmonic partial strengths:
\begin{verbatim} 

> compSine :: Tbl -> [PStrength] -> ScoreStmt
> compSine n s = CSTable 6 0 8192 True (compSine1 s)

\end{verbatim} 

\subsubsection{Naming Instruments and Tables}

In CSound, each table and instrument has a unique identifying integer
associated with it.  Haskore, on the other hand, uses strings to name
instruments.  What we need is a way to convert Haskore instrument
names to identifier integers that CSound can use.  Similar to
Haskore's player maps, we define a notion of a {\em CSound name map}
for this purpose.
\begin{verbatim} 

> type NameMap = [(Name, Int)]
> type Name    = String

\end{verbatim} 
A name map can be provided directly in the form 
{\tt [("name1", int1), ("name2", int2), ...]}, or the programmer can
define auxiliary functions to make map construction easier.
For example:
\begin{verbatim} 

> makeNameMap :: [Name] -> NameMap
> makeNameMap nms = zip nms [1..]

\end{verbatim} 
The following function will add a name to an existing name map.  If
the name is already in the map, an error results.
\begin{verbatim} 

> addToMap :: NameMap -> Name -> Int -> NameMap 
> addToMap nmap nm i = 
>   case (getId nmap nm) of
>     Nothing -> (nm,i) : nmap
>     Just _  -> error (" addToMap: the name " ++ nm ++ 
>                                     " is already in the name map")
>
> getId :: NameMap -> Name -> Maybe Int
> getId nmap nm =
>   case (find (\(n,_) -> n==nm) nmap) of
>     Nothing    -> Nothing
>     Just (n,i) -> Just i

\end{verbatim} 
Note the use of the function {\tt find} imported from the {\tt List}
library.

\subsubsection{Converting Haskore Music to a CSound Score File}

To convert a {\tt Music} value into a CSound score file, we need to:
\begin{enumerate} 
\item Convert the {\tt Music} value to a {\tt Performance}.
\item Convert the {\tt Performance} value to a {\tt Score}.
\item Write the {\tt Score} value to a CSound score file.
\end{enumerate} 

We already know how to do the first step.  Steps two and three will be
achieved by the following two functions:
\begin{verbatim} 

  perfToScore :: NameMap -> Performance -> Score
  writeScore  :: Score -> IO ()

\end{verbatim} 
The three steps can be put together in whatever way the user wishes,
but the most general way would be this:
\begin{verbatim} 

> musicToCSound :: NameMap -> PMap -> Tables -> Context -> Music -> IO ()
> musicToCSound nmap pmap tables cont m =
>   writeScore (tables ++ perfToScore nmap (perform pmap cont m))
>
> type Tables = Score

\end{verbatim} 
The {\tt Tables} argument is a user-defined set of function tables,
represented as a sequence of {\tt ScoreStmt}s (specifically, {\tt
CSTable} constructors).  (See Section \ref{function-tables-sect}.)

\paragraph*{From Performance to Score}

The translation between performance {\tt Events} and score {\tt
CSNotes} is straightforward, the only tricky parts being:
\begin{itemize} 
\item The unit of time in a {\tt Performance} is the second, whereas
in a {\tt Score} it is the beat.  However, the default CSound tempo is
60 beats per minute, or one beat per second, as was already mentioned,
and we use this default for our {\em Score} files.  Thus the two are
equivalent, and no translation is necessary.
\item In a {\tt Performance}, pitch is represented as an {\tt
AbsPitch} value, an integer denoting the absolute position of the
pitch in semitones (MIDI uses the same representation).  CSound,
however, uses different pitch representations, one of them being {\em
octave point pitch class}, or ``pch.''  Using pch, a pitch is
represented by a decimal number whose integer part represents the
octave, and decimal part represents the semitone within the octave
(thus 0.00 is the lowest C, 0.11 the lowest B, 8.00 is middle C,
etc.).  The function {\tt absToPch} defined below performs the
required conversion for us.
\end{itemize} 

\begin{verbatim} 

> perfToScore :: NameMap -> Performance -> Score
> perfToScore _ [] = []
> perfToScore nmap (Event t i p d v pfs : evs) =
>   case (getId nmap i) of
>     Nothing  -> error ("perfToScore: instrument " ++ i ++ " is unknown")
>     Just num -> CSNote num t d (absToPch p) v pfs : perfToScore nmap evs
>
> absToPch :: AbsPitch -> Pch
> absToPch ap = float (ap `quot` 12) + (float (ap `mod` 12) / 100.0)

\end{verbatim} 

\paragraph*{From Score to Score File}

Now that we have a value of type {\tt Score}, we must write it into a
plain text ASCII file with an extension {\tt .sco} in a way that
CSound will recognize.  This is done by the following function:
\begin{verbatim} 

> writeScore :: Score -> IO ()
> writeScore s = do putStr "\nName your score file "
>                   putStr "(.sco extension will be added): "
>                   name   <- getLine
>                   h      <- openFile (name ++ ".sco") WriteMode
>                   printScore h s
>                   hClose h

\end{verbatim} 
This function asks the user for the name of the score file, opens that
file for writing, writes the score into the file using the function
{\tt printScore}, and then closes the file.

The score file is a plain text file containing one statement per line.
Each statement consists of an opcode, which is a single letter that
determines the action to be taken, and a number of arguments.  The
opcodes we will use are ``e'' for end of score, ``t'' to set tempo,
``f'' to create a function table, and ``i'' for note events.
\begin{verbatim} 

> printScore :: Handle -> Score -> IO ()
> printScore h []       = hPutStr h "e\n"        -- end of score
> printScore h (s : ss) = do printStatement h s
>                            printScore h ss

\end{verbatim} 

In the following we will come across several instances where
we will need to write a list of floating point numbers into the file,
one number at a time, separated by spaces.  To do this, we will need 
to convert the list to a string.  This is done by the following 
function:
\begin{verbatim} 

> listToString :: [Float] -> String
> listToString []       = ""
> listToString (n : ns) = " " ++ show n ++ listToString ns

\end{verbatim} 

Finally, the {\tt printStatement} function:
\begin{verbatim} 

> printStatement :: Handle -> ScoreStmt -> IO ()
> printStatement h (CSTempo t) = 
>   hPutStr h ("t 0 " ++ show t ++ "\n")
> printStatement h (CSNote i st d p v pfs) = 
>   hPutStr h 
>     ("i " ++ show i ++ " " ++ show st ++ " " ++ show d ++ " " ++
>              show p ++ " " ++ show v ++ listToString pfs ++ "\n")
> printStatement h (CSTable t ct s n gr)   = 
>   hPutStr h
>     ("f " ++ show t ++ " " ++ show ct ++ " " ++ show s ++  
>       (if n then " " else " -") ++ showGenRoutine gr ++ "\n")
>   where showGenRoutine (SoundFile nm st cn) = 
>                        "1 " ++ nm ++ " " ++ show st ++ " 0 " ++ show cn
>         showGenRoutine (GenRoutine gn gas) =
>                        show gn ++ listToString gas

\end{verbatim} 

\subsection{The Orchestra File}
\label{orchestra-file-sect}

The orchestra file consists of two parts: a {\em header}, and one or
more {\em instrument blocks}.  The header sets global parameters
controlling sampling rate, control rate, and number of output
channels.  The instrument blocks define instruments, each identified
by a unique integer ID, and containing statements modifying or
generating various audio signals.  Each note statement in a score file
passes all its arguments---including the p-fields---to its
corresponding instrument in the orchestra file.  While some properties
vary from note to note, and should therefore be designed as p-fields,
many can be defined within the instrument; the choice is up to the
user.

The orchestra file is represented as:
\begin{verbatim} 

> type Orchestra = (Header, [InstBlock])

\end{verbatim} 
The orchestra header sets the audio rate, control rate, and number of
output channels:
\begin{verbatim} 

> type Header = (AudRate, CtrlRate, Chnls)
>
> type AudRate  = Int  -- samples per second
> type CtrlRate = Int  -- samples per second
> type Chnls    = Int  -- mono=1, stereo=2, surround=4

\end{verbatim} 
Digital computers represent continuous analog audio waveforms as a
sequence of discrete samples.  The audio rate ({\tt AudRate}) is the
number of these samples calculated each second.  Theoretically, the
maximum frequency that can be represented is equal to one-half the
audio rate.  Audio CDs contain 44,100 samples per second of music,
giving them a maximum sound frequency of 22,050 Hz, which is as high
as most human ears are able to hear.

Computing 44,100 values each second can be a demanding task for a CPU,
even by today's standards.  However, some signals used as inputs to
other signal generating routines don't require such a high resolution,
and can thus be generated at a lower rate.  A good example of this is
an amplitude envelope, which changes relatively slowly, and thus can
be generated at a rate much lower than the audio rate.  This rate is
called the {\em control rate} ({\tt CtrlRate}), and is set in the
orchestra file header.  The audio rate is usually a multiple of the
control rate, but this is not a requirement.

Each instrument block contains a unique identifying integer, as well
as an {\em orchestra expression} that defines the audio signal
characterizing that instrument:
\begin{verbatim} 

> type InstBlock = (Inst, OrcExp)

\end{verbatim} 
Recall that {\tt Inst} is a type synonym for an {\tt Int}.  This value
may be obtained from a string name and a name map using the function
{\tt getId :: NameMap -> Name -> Maybe Int} discussed earlier.

\subsubsection{Orchestra Expressions}

The data type {\tt OrcExp} is the largest deviation that we will make
from the actual CSound design.  In CSound, instruments are defined
using a sequence of statements that, in a piecemeal manner, define the
various oscillators, summers, constants, etc.\ that make up an
instrument.  These pieces can be given names, and these names can be
referenced from other statements.  But despite this rather imperative,
statement-oriented approach, it is acually completely functional.  In
other words, every CSound instrument can be rewritten as a single
expression.  It is this ``expression language'' that we capture in
{\tt OrcExp}.  A pleasant attribute of the result is that CSound's ad
hoc naming mechanism is replaced with Haskell's conventional way of
naming things.

The entire {\tt OrcExp} data type declaration is shown in Figure
\ref{OrcExp-fig}.  In what follows, we describe each of the various
constructors in turn.

\begin{figure}
{\scriptsize\vspace{-.7in}
\begin{verbatim}

> data OrcExp = Const Float
>             | Pfield Int
>
>             | Plus    OrcExp OrcExp
>             | Minus   OrcExp OrcExp
>             | Times   OrcExp OrcExp
>             | Divide  OrcExp OrcExp
>             | Power   OrcExp OrcExp
>             | Modulo  OrcExp OrcExp
>
>             | Int     OrcExp
>             | Frac    OrcExp
>             | Neg     OrcExp
>             | Abs     OrcExp
>             | Sqrt    OrcExp
>             | Sin     OrcExp
>             | Cos     OrcExp
>             | Exp     OrcExp
>             | Log     OrcExp
>
>             | AmpToDb OrcExp
>             | DbToAmp OrcExp
>             | PchToHz OrcExp
>             | HzToPch OrcExp
>
>             | GreaterThan   OrcExp OrcExp OrcExp OrcExp
>             | LessThan      OrcExp OrcExp OrcExp OrcExp
>             | GreaterOrEqTo OrcExp OrcExp OrcExp OrcExp
>             | LessOrEqTo    OrcExp OrcExp OrcExp OrcExp
>             | Equals        OrcExp OrcExp OrcExp OrcExp
>             | NotEquals     OrcExp OrcExp OrcExp OrcExp
>
>             | MonoOut       OrcExp
>             | LeftOut       OrcExp
>             | RightOut      OrcExp
>             | StereoOut     OrcExp OrcExp
>             | FrontLeftOut  OrcExp
>             | FrontRightOut OrcExp
>             | RearRightOut  OrcExp
>             | RearLeftOut   OrcExp
>             | QuadOut       OrcExp OrcExp OrcExp OrcExp
>
>             | Line     EvalRate Start Durn Finish
>             | Expon    EvalRate Start Durn Finish
>             | LineSeg  EvalRate Start Durn Finish [(Durn, Finish)]
>             | ExponSeg EvalRate Start Durn Finish [(Durn, Finish)]
>             | Env      EvalRate Sig RTime Durn DTime RShape SAttn DAttn Steep
>             | Phasor   EvalRate Freq InitPhase
>             | TblLookup       EvalRate Index Table IndexMode
>             | TblLookupI EvalRate Index Table IndexMode
>             | Osc       EvalRate Amp Freq Table
>             | OscI EvalRate Amp Freq Table
>             | FMOsc       Amp Freq CarFreq ModFreq ModIndex Table
>             | FMOscI Amp Freq CarFreq ModFreq ModIndex Table
>             | SampOsc     Amp Freq Table
>             | Random       EvalRate Amp 
>             | RandomHold   EvalRate Amp HoldHz
>             | RandomI EvalRate Amp HoldHz
>             | GenBuzz Amp Freq NumHarms LoHarm Multiplier Table
>             | Buzz    Amp Freq NumHarms Table
>             | Pluck   Amp Freq Table DecayMethod DecArg1 DecArg2
>             | Delay   MaxDel AudioSig
>             | DelTap  TapTime DelLine
>             | DelTapI TapTime DelLine
>             | DelayW  AudioSig
>             | Comb    AudioSig RevTime LoopTime
>             | AlPass  AudioSig RevTime LoopTime
>             | Reverb  AudioSig RevTime 
>      deriving (Show, Eq)

\end{verbatim}}
\caption{The OrcExp Data Type}
\label{OrcExp-fig}
\end{figure}

\paragraph*{Constants}

{\tt Const x} represents the floating-point constant {\tt x}.

\paragraph*{P-field Arguments}

{\tt Pfield n} refers to the $n$th p-field argument.  Recall that all
note characteristics, including pitch, volume, and duration, are
passed into the orchestra file as p-fields.  For example, to access
the pitch, one would write {\tt Pfield 4}.  To make the access of
these most common p-fields easier, we define the following constants:
\begin{verbatim} 

> noteDur, notePit, noteVol :: OrcExp
> noteDur = Pfield 3
> notePit = Pfield 4
> noteVol = Pfield 5

\end{verbatim} 

It is also useful to define the following standard names, which are
identical to those used in CSound:

\begin{verbatim} 

> p1,p2,p3,p4,p5,p6,p7,p8,p9 :: OrcExp
> p1 = Pfield 6
> p2 = Pfield 7
> p3 = Pfield 8
> p4 = Pfield 9
> p5 = Pfield 10
> p6 = Pfield 11
> p7 = Pfield 12
> p8 = Pfield 13
> p9 = Pfield 14

\end{verbatim} 

\paragraph*{Arithmetic and Transcendental Functions}

Arithmetic expressions are represented by the constructors {\tt Plus},
{\tt Minus}, {\tt Times}, {\tt Divide}, {\tt Power}, and {\tt Modulo},
each taking two {\tt OrcExps} as arguments.  In addition, there are a
number of unary arithmatic functions: {\tt Int x} and {\tt Frac x}
represent the integer and fractional parts, respectively, of {\tt x}.
{\tt Abs x}, {\tt Neg x}, {\tt Sqrt x}, {\tt Sin x}, and {\tt Cos x}
represent the absolute value, negation, square root, sine and cosine
(in radians) of {\tt x}, respectively.  {\tt Exp x} represents $e$
raised to the power {\tt x}, and {\tt Log x} is the natural logarithm
of {\tt x}.

To facilitate the use of these arithmetic functions, we can make {\tt
OrcExp} an instance of certain numeric type classes, thus providing
more conventional names for the various operations.
\begin{verbatim} 

> instance Num OrcExp where
>   (+)    = Plus
>   (-)    = Minus
>   (*)    = Times
>   negate = Neg
>   abs    = Abs
>   fromInteger i = Const (fromInteger i)
>   -- omitted: signum

> instance Fractional OrcExp where
>   (/) = Divide
>   fromRational x = Const (fromRational x)

> instance Floating OrcExp where
>  exp   = Exp
>  log   = Log
>  sqrt  = Sqrt
>  (**)  = Power
>  sin   = Sin
>  cos   = Cos
>  pi    = Const pi
>  tan x = Sin x / Cos x
>  -- omitted: asin, acos, atan    :: a -> a
>  --          sinh, cosh, tanh    :: a -> a
>  --          asinh, acosh, atanh :: a -> a

\end{verbatim} 

For example, {\tt Plus (Pfield 3) (Power (Sin (Pfield 6)) (Const 2))}
can now be written simply as {\tt noteDur + sin p6 ** 2}.

\paragraph*{Pitch and Volume Coercions}

The next set of constructors represent functions that convert between
different CSound pitch and volume representations.  Recall that the
{\tt CSNote} constructor uses decibels for volume and ``pch'' notation
for pitch.  If these values are to be used as inputs into a signal
generating or modifying routine, they must first be converted into
units of raw amplitude and hertz, respectively.  This is accomplished
by the functions represented by {\tt DbToAmp} and {\tt PchToHz},
and their inverses are {\tt AmpToDb} and {\tt HzToPch}.  Thus note
that {\tt PchToHz (notePit + 0.01)} raises the pitch by one semitone,
whereas {\tt PchToHz notePit + 0.01} raises the pitch by 0.01 Hz.

\paragraph*{Comparison Operators}

{\tt OrcExp} also includes comparison constructors {\tt GreaterThan},
{\tt LessThan}, {\tt GreaterOrEqTo}, {\tt LessOrEqTo}, {\tt Equals},
and {\tt NotEquals}.  Each takes four {\tt OrcExp} arguments: the
values of the first two are compared, and if the result is true, the
expression evaluates to the third argument; otherwise, it takes on the
value of the fourth.\footnote{This design emulates that of CSound.  A
more conventional design would have comparison operators that return a
Boolean value, and a conditional expression to choose between two
values based on a Boolean.  One could then add Boolean operators such
as ``and'', ``or'', etc.  It seems possible to do this in Haskore, but
its translation into CSound would be more difficult, and thus we take
the more conservative approach for now.}

\paragraph*{Output Operators}

The next group of constructors represent CSound's {\em output
statements}.  The constructors are {\tt MonoOut}, {\tt LeftOut}, {\tt
RightOut}, {\tt StereoOut}, {\tt FrontLeftOut}, {\tt FrontRightOut},
{\tt RearRightOut}, {\tt RearLeftOut}, and {\tt QuadOut}.  {\tt
StereoOut} takes two {\tt OrcExp} arguments, {\tt QuadOut} takes four,
and the rest take one.

The top-level of an instrument's {\tt OrcExp} (i.e., the one in the
{\tt InstBlock} value) will normally be an application of one of
these.  Furthermore, the constructor used must be in agreement with
the number of output channels specified in the orchestra header---for
example, using {\tt LeftOut} when the header declares the resulting
sound file to be mono will result in an error.

\paragraph*{Signal Generation and Modification}

The most sophisticated {\tt OrcExp} constructors are those that
emulate CSound's signal generation and modification functions.  There
are quite a few of them, and they are all described here, although the
reader is encouraged to read the CSound manual for further details.

Before defining each constructor, however, there are two general
issues to discuss:

First, signals in CSound can be generated at three rates: the note
rate (i.e., with every note event), the control rate, and the audio
rate (we discussed the latter two earlier).  Many of the signal
generating routines can produce signals at more than one rate, so the
rate must be specified as an argument.  The following simple data
structure serves this purpose:
\begin{verbatim} 

> data EvalRate = NR  -- note rate
>               | CR  -- control rate
>               | AR  -- audio rate
>      deriving (Show, Eq)

\end{verbatim} 

Second, note in Figure \ref{OrcExp-fig} that this collection of
constructors uses quite a few other type names.  In all cases,
however, these are simply type synonyms for {\tt OrcExp}, and are used
only for clarity.  These type synonyms are listed here in one fell
swoop:
\begin{verbatim} 

> type Start       = OrcExp
> type Durn        = OrcExp
> type Finish      = OrcExp
> type Sig         = OrcExp
> type RTime       = OrcExp
> type DTime       = OrcExp
> type RShape      = OrcExp
> type SAttn       = OrcExp
> type DAttn       = OrcExp
> type Steep       = OrcExp
> type Freq        = OrcExp
> type InitPhase   = OrcExp
> type Index       = OrcExp
> type Table       = OrcExp
> type IndexMode   = OrcExp
> type Amp         = OrcExp
> type CarFreq     = OrcExp
> type ModFreq     = OrcExp
> type ModIndex    = OrcExp
> type HoldHz      = OrcExp
> type NumHarms    = OrcExp
> type LoHarm      = OrcExp
> type Multiplier  = OrcExp
> type DecayMethod = OrcExp
> type DecArg1     = OrcExp
> type DecArg2     = OrcExp
> type MaxDel      = OrcExp
> type AudioSig    = OrcExp
> type TapTime     = OrcExp
> type DelLine     = OrcExp
> type RevTime     = OrcExp
> type LoopTime    = OrcExp

\end{verbatim} 

We can now discuss each constructor in turn:
\begin{enumerate} 
\item {\tt Line evalrate start durn finish} produces values along a
straight line from {\tt start} to {\tt finish}.  The values can be
generated either at control or audio rate, and the line covers a
period of time equal to {\tt durn} seconds.

\item {\tt Expon} is similar to {\tt Line}, but 
{\tt Expon evalrate start durn finish} produces an exponential curve
instead of a straight line.  

\item If a more elaborate signal is required, one can use the
constructors {\tt LineSeg} or {\tt ExponSeg}, which take arguments of
type {\tt EvalRate}, {\tt Start}, {\tt Durn}, and {\tt Finish}, as
above, and also {\tt [(Durn, Finish)]}.  The first four arguments work
as before, but only for the first of a number of segments.  The
subsequent segment lengths and endpoints are given in the fifth
argument.  A signal containing both straight line and exponential
segments can be obtained by adding a {\tt LineSeg} signal and {\tt
ExponSeg} signal together in an appropriate way.

\item {\tt Env evalrate sig rtime durn dtime rshape sattn dattn steep}
modifies the signal {\tt sig} by applying an envelope to
it.\footnote{Although this function is widely-used in CSound, the same
effect can be accomplished by creating a signal that is a combination
of straight line and exponential curve segments, and multiplying it by
the signal to be modified.}  {\tt rtime} and {\tt dtime} are the rise
time and decay time, respectively (in seconds), and {\tt durn} is the
overall duration.  {\tt rshape} is the identifier integer of a
function table storing the rise shape.  {\tt sattn} is the
pseudo-steady state attenuation factor.  A value between 0 and 1 will
cause the signal to exopnentially decay over the steady period, a
value greater than 1 will cause the signal to exponentially rise, and
a value of 1 is a true steady state maintained at the last rise value.
{\tt steep}, whose value is usually between $-0.9$ and $+0.9$,
influences the steepness of the exponential trajectory.  {\tt dattn}
is the attenuation factor by which the closing steady state value is
reduced exponentially over the decay period, with value usually around
0.01.

\item {\tt Phasor evalrate freq initphase} generates a signal moving
from 0 to 1 at a given frequency and starting at the given initial
phase offset.  When used properly as the index to a table lookup unit,
{\tt Phasor} can simulate the behavior of an oscillator.

\item Table lookup constructors {\tt TblLookup} and {\tt
TblLookupI} both take {\tt EvalRate}, {\tt Index}, {\tt Table},
and {\tt IndexMode} arguments.  The {\tt IndexMode} is either 0 or 1,
differentiating between raw index and normalized index (zero to one);
for convenience we define:
\begin{verbatim} 

> rawIndex, normalIndex :: OrcExp
> rawIndex    = 0.0
> normalIndex = 1.0

\end{verbatim} 

Both {\tt TblLookup} and {\tt TblLookupI} return values stored in
the specified table at the given index.  The difference is that {\tt
TblLookupI} uses the fractional part of the index to interpolate
between adjacent table entries, which generates a smoother signal at a
small cost in execution time.

As mentioned, the output of a {\tt Phasor} can be used as input to a
table lookup to simulate an oscillator whose frequencey is controlled
by the note pitch.  This can be accomplished easily by the following
piece of Haskore code:
\begin{verbatim} 
  osc = let index = Phasor AR (PchToHz notePit) 0.0
        in  TblLookupI AR index table normalIndex
\end{verbatim} 
where {\tt table} is some given function table ID.  If {\tt osc} is
given as argument to an output operator such as {\tt MonoOut}, then
this {\tt OrcExp} coupled with an instrument ID number (say, 1)
produces a complete instrument block:
\begin{verbatim} 
  i1 = (1, MonoOut osc)
\end{verbatim} 
Adding a suitable {\tt Header} would then give us a complete, though
somewhat sparse, {\tt Orchestra} value.

\item Instead of the above design we could use one of the built-in
CSound oscillators, {\tt Osc} and {\tt OscI}, which differ in the
same way as {\tt TblLookup} and {\tt TblLookupI}.  Each
oscillator constructor takes arguments of type {\tt EvalRate}, {\tt
Amp} (in raw amplitude), {\tt Freq} (in Hertz), and {\tt Table}.  The
result is a signal that oscillates through the function table at the
given frequency.  Thus the following value is equivalent to {\tt osc}
above:
\begin{verbatim} 
  osc' = OscI AR 1 (PchToHz notePit) table
\end{verbatim} 

\item It is often desirable to use the output of one oscillator to
modulate the frequency of another, a process known as {\em frequency
modulation}.  {\tt FMOsc amp freq carfreq modfreq modindex table} is a
signal whose effective modulating frequency is {\tt freq*modfreq}, and
whose carrier frequency is {\tt freq*carfreq}.  {\tt modindex} is the
{\em index of modulation}, usually a value between 0 and 4, which
determines the timbre of the resulting signal.  {\tt FMOscI}
behaves similarly.  Note that there is no {\tt EvalRate} argument,
since these functions work at audio rate only.  The given function
table normally contains a sine wave.  This oscillator setup is known
as the {\em chowning FM} setup.

\item {\tt SampOsc amp freq table} oscillates through a table
containing an AIFF sampled sound segment.  This is the only time a
table can have a length that is not a power of two, as mentioned
earlier.  Like {\tt FMOsc}, {\tt SampOsc} can only generate values at
the audio rate.

\item {\tt Random evalrate amp} produces a random number series
between {\tt -amp} and {\tt +amp} at either control or audio rate.
{\tt RandomHold evalrate amp holdhz} does the same but will hold each
number for {\tt holdhz} cycles before generating a new one.  {\tt
RandomI evalrate amp holdhz} will in addition provide straight
line interpolation between successive numbers.

All the remaining constructors only operate at audio rate, and thus do
not have {\tt EvalRate} arguments.

\item {\tt GenBuzz amp freq numharms loharm multiplier table}
generates a signal that is an additive set of harmonically related
cosine partials.  {\tt freq} is the fundamental frequency, {\tt
numharms} is the number of harmonics, and {\tt loharm} is the lowest
harmonic present.  The amplitude coefficients of the harmonics are
given by the exponential series {\tt a}, {\tt a * multiplier}, 
{\tt a * multiplier**2}, $\ldots$ , {\tt a * multiplier**(numharms-1)}.
The value {\tt a} is chosen so that the sum of the amplitudes is
{\tt amp}.  {\tt table} is a function table containing a cosine wave.

\item {\tt Buzz} is a special case of {\tt GenBuzz} in which {\tt
loharm = 1.0} and {\tt Multiplier = 1.0}.  {\tt table} is a function
table containing a sine wave.

Note that the above two constructors have an analog in the generating
routine GEN11 and the related function {\tt cosineHarms} (see Section
\ref{function-table-sect}).  {\tt cosineHarms} stores into a table the
same waveform that would be generated by {\tt Buzz} or {\tt GenBuzz}.
However, although {\tt cosineHarms} is more efficient, it has fixed
arguments and thus lacks the flexibility of {\tt Buzz} and {\tt
GenBuzz} in being able to vary the argument values with time.

\item {\tt Pluck amp freq table decaymethod decarg1 decarg2} is an
audio signal that simulates a plucked string or drum sound,
constructed using the Karplus-Strong algorithm.  The signal has
amplitude {\tt amp} and frequency {\tt freq}.  It is produced by
iterating through an internal buffer that initially contains a copy of
{\tt table} and is smoothed on every pass to simulate the natural
decay of a plucked string.  If 0.0 is used for {\tt table}, then the
initial buffer is filled with a random sequence.  There are six
possible decay modes:
\begin{enumerate} 
\item {\em simple smoothing}, which ignores the two arguments;
\item {\em stretched smoothing}, which stretches the smoothing time by
a factor of {\tt decarg1}, ignoring {\tt decarg2};
\item {\em simple drum}, where {\tt decarg1} is a ``roughness factor''
(0 for pitch, 1 for white noise; a value of 0.5 gives an optimal snare
drum sound);
\item {\em stretched drum}, which contains both roughness ({\tt
decarg1}) and stretch ({\tt decarg2}) factors;
\item {\em weighted smoothing}, in which {\tt decarg1} gives the
weight of the current sample and {\tt decarg2} the weight of the
previous one ({\tt decarg1+decarg2} must be $\leq1$); and
\item {\em recursive filter smoothing}, which ignores both arguments.
\end{enumerate} 
Here again are some helpful constants:
\begin{verbatim} 

> simpleSmooth, stretchSmooth, simpleDrum, stretchDrum, 
>   weightedSmooth, filterSmooth :: OrcExp
> simpleSmooth   = 1.0
> stretchSmooth  = 2.0
> simpleDrum     = 3.0
> stretchDrum    = 4.0
> weightedSmooth = 5.0
> filterSmooth   = 6.0

\end{verbatim} 

\item {\tt Delay deltime audiosig} establishes a digital delay line,
where {\tt audiosig} is the source, and {\tt deltime} is the delay
time in seconds.  

The delay line can also be {\em tapped} by {\tt DelayTap deltime
delline} and {\tt DelayTapI deltime delline}, where {\tt deltime} is
the tap delay, and {\tt delline} must be a delay line created by the
{\tt Delay} constructor above.  Again, {\tt DelayTapI} uses
interpolation, and may take up to twice as long as {\tt DelayTap} to
run, but produces higher precision results and thus a cleaner signal.

(Note: the constructor {\tt DelayW} is used in the translation
described later to mark the end of a sequence of delay taps, and is
not intended for use by the user.)

\item Reverberation can be added to a signal using 
{\tt Comb audiosig revtime looptime}, 
{\tt AlPass audiosig revtime looptime}, and 
{\tt Reverb audiosig revtime}.  {\tt revtime} is the time in seconds
it takes a signal to decay to 1/1000th of its original amplitude, and
{\tt looptime} is the echo density.  {\tt Comb} produces a ``colored''
reverb, {\tt AlPass} a ``flat'' reverb, and {\tt Reverb} a ``natural
room'' reverb.
\end{enumerate}

\subsubsection{Converting Orchestra Values to Orchestra Files}

We must now convert the {\tt OrcExp} values into a form which can be
written into a CSound {\tt .sco} file.  As mentioned earlier, each
signal generation or modification statement in CSound assigns its
result a string name.  This name is used whenever another statement
takes the signal as an argument.  Names of signals generated at note
rate must begin with the letter ``i'', control rate with letter ``k'',
and audio rate with letter ``a''.  The output statements do not
generate a signal so they do not have a result name.

\begin{figure}
{\scriptsize\vspace{-.9in}
\begin{verbatim} 

> mkList :: OrcExp -> [(EvalRate, OrcExp)]
> mkList (Const _)                   = []
> mkList (Pfield _)                  = []
> mkList (x1 `Plus` x2)              = mkListAll [x1,x2]
> mkList (x1 `Minus` x2)             = mkListAll [x1,x2]
> mkList (x1 `Times` x2)             = mkListAll [x1,x2]
> mkList (x1 `Divide` x2)            = mkListAll [x1,x2]
> mkList (x1 `Power` x2)             = mkListAll [x1,x2]
> mkList (x1 `Modulo` x2)            = mkListAll [x1,x2]
> mkList (Int x)                     = mkList x
> mkList (Frac x)                    = mkList x
> mkList (Abs x)                     = mkList x
> mkList (Neg x)                     = mkList x
> mkList (Sqrt x)                    = mkList x
> mkList (Sin x)                     = mkList x
> mkList (Cos x)                     = mkList x
> mkList (Exp x)                     = mkList x
> mkList (Log x)                     = mkList x
> mkList (AmpToDb x)                 = mkList x
> mkList (DbToAmp x)                 = mkList x
> mkList (PchToHz x)                 = mkList x
> mkList (HzToPch x)                 = mkList x
> mkList (GreaterThan   x1 x2 x3 x4) = mkListAll [x1,x2,x3,x4]
> mkList (LessThan      x1 x2 x3 x4) = mkListAll [x1,x2,x3,x4]
> mkList (GreaterOrEqTo x1 x2 x3 x4) = mkListAll [x1,x2,x3,x4]
> mkList (LessOrEqTo    x1 x2 x3 x4) = mkListAll [x1,x2,x3,x4]
> mkList (Equals        x1 x2 x3 x4) = mkListAll [x1,x2,x3,x4]
> mkList (NotEquals     x1 x2 x3 x4) = mkListAll [x1,x2,x3,x4]
> mkList (QuadOut       x1 x2 x3 x4) = mkListAll [x1,x2,x3,x4]
> mkList (StereoOut x1 x2)           = mkListAll [x1,x2]
> mkList (MonoOut x)                 = mkList x
> mkList (LeftOut x)                 = mkList x
> mkList (RightOut x)                = mkList x
> mkList (FrontLeftOut x)            = mkList x
> mkList (FrontRightOut x)           = mkList x
> mkList (RearRightOut x)            = mkList x
> mkList (RearLeftOut x)             = mkList x
> mkList oe@(Line     er x1 x2 x3)   = (er,oe) : mkListAll [x1,x2,x3]
> mkList oe@(Expon    er x1 x2 x3)   = (er,oe) : mkListAll [x1,x2,x3]
> mkList oe@(LineSeg  er x1 x2 x3 xs)
>                   = (er,oe) : mkListAll (x1:x2:x3: flattenTuples2 xs)
> mkList oe@(ExponSeg er x1 x2 x3 xs)
>                   = (er,oe) : mkListAll (x1:x2:x3: flattenTuples2 xs)
> mkList oe@(Env er x1 x2 x3 x4 x5 x6 x7 x8) 
>                   = (er,oe) : mkListAll [x1,x2,x3,x4,x5,x6,x7,x8]
> mkList oe@(Phasor    er x1 x2)     = (er,oe) : mkListAll [x1,x2]
> mkList oe@(TblLookup er x1 x2 x3)  = (er,oe) : mkListAll [x1,x2,x3]
> mkList oe@(TblLookupI er x1 x2 x3) = (er,oe) : mkListAll [x1,x2,x3]
> mkList oe@(Osc       er x1 x2 x3)  = (er,oe) : mkListAll [x1,x2,x3]
> mkList oe@(OscI      er x1 x2 x3)  = (er,oe) : mkListAll [x1,x2,x3]
> mkList oe@(Random       er x)      = (er,oe) : mkList x
> mkList oe@(RandomHold   er x1 x2)  = (er,oe) : mkListAll [x1,x2]
> mkList oe@(RandomI      er x1 x2)  = (er,oe) : mkListAll [x1,x2]
> mkList oe@(FMOsc x1 x2 x3 x4 x5 x6)= (AR,oe) : mkListAll [x1,x2,x3,x4,x5,x6]
> mkList oe@(FMOscI x1 x2 x3 x4 x5 x6) 
>                                    = (AR,oe) : mkListAll [x1,x2,x3,x4,x5,x6]
> mkList oe@(SampOsc x1 x2 x3)       = (AR,oe) : mkListAll [x1,x2,x3]
> mkList oe@(GenBuzz x1 x2 x3 x4 x5 x6) 
>                                    = (AR,oe) : mkListAll [x1,x2,x3,x4,x5,x6]
> mkList oe@(Buzz x1 x2 x3 x4)       = (AR,oe) : mkListAll [x1,x2,x3,x4]
> mkList oe@(Pluck x1 x2 x3 x4 x5 x6)= (AR,oe) : mkListAll [x1,x2,x3,x4,x5,x6]
> mkList oe@(Delay x1 x2)            = (AR,oe) : mkListAll [x1,x2]
> mkList oe@(DelTap x1 x2)           = (AR,oe) : mkList x2
> mkList oe@(DelTapI x1 x2)          = (AR,oe) : mkList x2
> mkList oe@(DelayW x)               = error "DelayW not for you!"
> mkList oe@(Comb x1 x2 x3)          = (AR,oe) : mkListAll [x1,x2,x3]
> mkList oe@(AlPass x1 x2 x3)        = (AR,oe) : mkListAll [x1,x2,x3]
> mkList oe@(Reverb x1 x2)           = (AR,oe) : mkListAll [x1,x2]

\end{verbatim}}
\caption{The {\tt mkList} Function}
\label{mkList-fig}
\end{figure}

The function {\tt mkList} is shown in Figure \ref{mkList-fig}, and
creates an entry in the list for every signal generating, modifying,
or outputting constructor.  It uses the following auxiliary functions:
\begin{verbatim} 

> mkListAll :: [OrcExp] -> [(EvalRate, OrcExp)]
> mkListAll = foldr (++) [] . map mkList
>
> addNames :: [(EvalRate,OrcExp)] -> [(Name,OrcExp)]
> addNames ls = zipWith counter ls [1..]
>                 where counter (er,x) n = 
>                         let var = case er of 
>                                     AR -> 'a' : show n
>                                     CR -> 'k' : show n
>                                     NR -> 'i' : show n
>                         in (var,x)

\end{verbatim} 

Putting all of the above together, here is a function that converts an
{\tt OrcExp} into a list of proper name / {\tt OrcExp} pairs.  Each
one of these will eventually result in one statement in the CSound
orchestra file.  (The result of {\tt mkList} is reversed to ensure
that a definition exists before it is used; and this must be done {\em
before} {\tt nub} is applied (which removes duplicates), for the same
reason.)
\begin{verbatim} 

> processOrcExp :: OrcExp -> [(Name, OrcExp)]
> processOrcExp = addNames . nub . procDelay . reverse . mkList

\end{verbatim} 

The function {\tt procDelay} is used to process delay lines, which
require special treatment because a delay line followed by a number of
taps must be ended in Csound with a {\tt DelayW} command.

\begin{verbatim} 

> procDelay :: [(EvalRate, OrcExp)] -> [(EvalRate, OrcExp)]
> procDelay []                           = []
> procDelay (x@(AR, d@(Delay _ _)) : xs) = [x] ++ procTaps d xs ++ procDelay xs
> procDelay (x : xs)                     = x : procDelay xs
>
> procTaps :: OrcExp -> [(EvalRate, OrcExp)] -> [(EvalRate, OrcExp)]
> procTaps d@(Delay t sig) []           = [(AR, DelayW sig)]
> procTaps d (x@(AR,DelTap t dl) : xs)  = 
>            if d == dl then (mkList t ++ [x] ++ procTaps d xs)
>                       else procTaps d xs
> procTaps d (x@(AR,DelTapI t dl): xs)  = 
>            if d == dl then (mkList t ++ [x] ++ procTaps d xs)
>                       else procTaps d xs
> procTaps d (x : xs)                   = procTaps d xs

\end{verbatim} 

The functions that follow are used to write the orchestra file.  {\tt
writeOrchestra} is similar to {\tt writeScore}: it asks the user for a
file name, opens the file, writes the given orchestra value to the
file, and then closes the file.
\begin{verbatim} 

> writeOrchestra :: Orchestra -> IO ()
> writeOrchestra orch = do putStr "\nName your orchestra file "
>                          putStr "(.orc extension will be added): "
>                          name <- getLine
>                          h    <- openFile (name ++ ".orc") WriteMode
>                          writeOrc h orch
>                          hClose h

\end{verbatim} 

{\tt writeOrc} splits the task of writing the orchestra into two parts:
writing the header and writing the instrument blocks
\begin{verbatim} 

> writeOrc :: Handle -> Orchestra -> IO ()
> writeOrc h (hdr,ibs) = do writeHeader  h hdr
>                           writeIBlocks h ibs                          

\end{verbatim} 
Writing the header is relatively simple, and is accomplished by the 
following function:
\begin{verbatim} 

> writeHeader :: Handle -> Header -> IO ()
> writeHeader h (a,k,nc) = 
>   hPutStrLn h (  "sr     = " ++ show a ++
>                "\nkr     = " ++ show k ++
>                "\nksmps  = " ++ show (float a / float k) ++
>                "\nnchnls = " ++ show nc)

\end{verbatim} 

{\tt writeIBlocks} writes each instrument block using the function
{\tt writeIBlock}:
\begin{verbatim} 

> writeIBlocks :: Handle -> [InstBlock] -> IO ()
> writeIBlocks h = mapM_ (writeIBlock h)

\end{verbatim} 

{\tt writeIBlock} writes a single instrument block.
\begin{verbatim} 

> writeIBlock :: Handle -> InstBlock -> IO ()
> writeIBlock h (num,ox) = 
>   do hPutStrLn    h ("\ninstr " ++ show num)
>      writeOrcExps h (processOrcExp ox ++ [("",ox)])
>      hPutStrLn    h "endin"

\end{verbatim} 

Recall that after processing, the {\tt OrcExp} becomes a list of {\tt
(Name, OrcExp)} pairs.  The last few functions write each of these
named {\tt OrcExp}s as a statement in the orchestra file.  Whenever a
signal generation/modification constructor is encountered in an
argument list of another constructor, the argument's string name is
used instead, as found in the list of {\tt (Name, OrcExp)} pairs.

\begin{figure}
{\scriptsize\vspace{-.7in}
\begin{verbatim}

> writeOrcExps :: Handle -> [(Name,OrcExp)] -> IO ()
> writeOrcExps h noes = mapM_ writeOrcExp noes 
>   where
>     writeOrcExp :: (Name,OrcExp) -> IO ()
>     writeOrcExp (nm, MonoOut x)           = hPutStr h "out "   >> writeArgs [x]
>     writeOrcExp (nm, LeftOut x)           = hPutStr h "outs1 " >> writeArgs [x]
>     writeOrcExp (nm, RightOut x)          = hPutStr h "outs2 " >> writeArgs [x]
>     writeOrcExp (nm, StereoOut x1 x2)     = hPutStr h "outs "  >> writeArgs [x1,x2]
>     writeOrcExp (nm, FrontLeftOut x)      = hPutStr h "outq1 " >> writeArgs [x]
>     writeOrcExp (nm, FrontRightOut x)     = hPutStr h "outq2 " >> writeArgs [x]
>     writeOrcExp (nm, RearRightOut x)      = hPutStr h "outq3 " >> writeArgs [x]
>     writeOrcExp (nm, RearLeftOut x)       = hPutStr h "outq4 " >> writeArgs [x]
>     writeOrcExp (nm, QuadOut x1 x2 x3 x4) = hPutStr h "outq "  >> writeArgs [x1,x2,x3,x4]
>     writeOrcExp (nm, Line er x1 x2 x3) = 
>       hPutStr h (nm ++ " line ")    >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, Expon er x1 x2 x3) = 
>       hPutStr h (nm ++ " expon ")   >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, LineSeg er x1 x2 x3 xlist)  = 
>       hPutStr h (nm ++ " linseg ")  >> writeArgs ([x1,x2,x3] ++ flattenTuples2 xlist)
>     writeOrcExp (nm, ExponSeg er x1 x2 x3 xlist) = 
>       hPutStr h (nm ++ " expseg ")  >> writeArgs ([x1,x2,x3] ++ flattenTuples2 xlist)
>     writeOrcExp (nm, Env er x1 x2 x3 x4 x5 x6 x7 x8) = 
>       hPutStr h (nm ++ " envlpx ")  >> writeArgs [x1,x2,x3,x4,x5,x6,x7,x8]
>     writeOrcExp (nm, Phasor er x1 x2) =
>       hPutStr h (nm ++ " phasor ")  >> writeArgs [x1,x2]
>     writeOrcExp (nm, TblLookup er x1 x2 x3) =
>       hPutStr h (nm ++ " table ")   >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, TblLookupI er x1 x2 x3) =
>       hPutStr h (nm ++ " tablei ")  >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, Osc er x1 x2 x3) = 
>       hPutStr h (nm ++ " oscil ")   >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, OscI er x1 x2 x3) = 
>       hPutStr h (nm ++ " oscili ")  >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, FMOsc x1 x2 x3 x4 x5 x6) = 
>       hPutStr h (nm ++ " foscil ")  >> writeArgs [x1,x2,x3,x4,x5,x6]
>     writeOrcExp (nm, FMOscI x1 x2 x3 x4 x5 x6) = 
>       hPutStr h (nm ++ " foscili ") >> writeArgs [x1,x2,x3,x4,x5,x6]
>     writeOrcExp (nm, SampOsc x1 x2 x3) = 
>       hPutStr h (nm ++ " loscil ")  >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, Random er x) = 
>       hPutStr h (nm ++ " rand ")    >> writeArgs [x]
>     writeOrcExp (nm, RandomHold er x1 x2) = 
>       hPutStr h (nm ++ " randh ")   >> writeArgs [x1,x2]
>     writeOrcExp (nm, RandomI er x1 x2) = 
>       hPutStr h (nm ++ " randi ")   >> writeArgs [x1,x2]
>     writeOrcExp (nm, GenBuzz x1 x2 x3 x4 x5 x6) = 
>       hPutStr h (nm ++ " gbuzz ")   >> writeArgs [x1,x2,x3,x4,x5,x6]
>     writeOrcExp (nm, Buzz x1 x2 x3 x4) = 
>       hPutStr h (nm ++ " buzz ")    >> writeArgs [x1,x2,x3,x4]
>     writeOrcExp (nm, Pluck x1 x2 x3 x4 x5 x6) = 
>       hPutStr h (nm ++ " pluck ")   >> writeArgs [x1,x2,x2,x3,x4,x5,x6]
>     writeOrcExp (nm, Delay x1 x2) = hPutStr h (nm ++ " delayr ") >> writeArgs [x1] 
>     writeOrcExp (nm, DelayW x) = hPutStr h ("    delayw ")     >> writeArgs [x]
>     writeOrcExp (nm, DelTap x1 x2)    = hPutStr h (nm ++ " deltap ")  >> writeArgs [x1]
>     writeOrcExp (nm, DelTapI x1 x2)   = hPutStr h (nm ++ " deltapi ") >> writeArgs [x1]
>     writeOrcExp (nm, Comb x1 x2 x3)   = hPutStr h (nm ++ " comb ")    >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, AlPass x1 x2 x3) = hPutStr h (nm ++ " alpass ")  >> writeArgs [x1,x2,x3]
>     writeOrcExp (nm, Reverb x1 x2) = hPutStr h (nm ++ " reverb ")  >> writeArgs [x1,x2]
>     writeOrcExp _ = error "writeOrcExp: unknown constructor\n"
>
>     writeArgs :: [OrcExp] -> IO ()
>     writeArgs [x]    = hPutStrLn h (showExp noes x)
>     writeArgs (x:xs) = hPutStr   h (showExp noes x ++ ", ") >> writeArgs xs

\end{verbatim}}
\caption{The Function {\tt writeOrcExp}}
\label{writeOrcExp-fig}
\end{figure}

\begin{figure}
\begin{verbatim}

> showExp :: [(Name, OrcExp)] -> OrcExp -> String
> showExp _ (Const x)         = show x 
> showExp _ (Pfield p)        = "p" ++ show p
> showExp xs (x1 `Plus` x2)   = showBin xs " + " x1 x2
> showExp xs (x1 `Minus` x2)  = showBin xs " - " x1 x2
> showExp xs (x1 `Times` x2)  = showBin xs " * " x1 x2
> showExp xs (x1 `Divide` x2) = showBin xs " / " x1 x2
> showExp xs (x1 `Power` x2)  = showBin xs " ^ " x1 x2
> showExp xs (x1 `Modulo` x2) = showBin xs " % " x1 x2
> showExp xs (Int x)          = "int("  ++ showExp xs x ++ ")"
> showExp xs (Frac x)         = "frac(" ++ showExp xs x ++ ")"
> showExp xs (Abs x)          = "abs("  ++ showExp xs x ++ ")"
> showExp xs (Neg x)          = "-("    ++ showExp xs x ++ ")"
> showExp xs (Sqrt x)         = "sqrt(" ++ showExp xs x ++ ")"
> showExp xs (Sin x)          = "sin("  ++ showExp xs x ++ ")"
> showExp xs (Cos x)          = "cos("  ++ showExp xs x ++ ")"
> showExp xs (Exp x)          = "exp("  ++ showExp xs x ++ ")"
> showExp xs (Log x)          = "log("  ++ showExp xs x ++ ")"
> showExp xs (AmpToDb x)      = "dbamp("  ++ showExp xs x ++ ")"
> showExp xs (DbToAmp x)      = "ampdb("  ++ showExp xs x ++ ")"
> showExp xs (PchToHz x)      = "cpspch(" ++ showExp xs x ++ ")"
> showExp xs (HzToPch x)      = "pchoct (octcps(" ++ showExp xs x ++ "))"
> showExp xs (GreaterThan x1 x2 x3 x4)   = showComp xs " > "  x1 x2 x3 x4
> showExp xs (LessThan    x1 x2 x3 x4)   = showComp xs " < "  x1 x2 x3 x4
> showExp xs (GreaterOrEqTo x1 x2 x3 x4) = showComp xs " >= " x1 x2 x3 x4
> showExp xs (LessOrEqTo x1 x2 x3 x4)    = showComp xs " <= " x1 x2 x3 x4
> showExp xs (Equals x1 x2 x3 x4)        = showComp xs " == " x1 x2 x3 x4
> showExp xs (NotEquals x1 x2 x3 x4)     = showComp xs " != " x1 x2 x3 x4
> showExp xs ox = case find (\(nm,oexp) -> ox==oexp) xs of
>                   Just (nm,_) -> nm
>                   Nothing     -> error ("showExp: constructor not found\n")
>
> showBin xs s x1 x2 =
>     "(" ++ showExp xs x1 ++ s ++ showExp xs x2 ++ ")"
> showComp xs s x1 x2 x3 x4 =
>     "(" ++ showExp xs x1 ++ s ++ showExp xs x2 ++ " ? " ++ 
>             showExp xs x3 ++ " : " ++ showExp xs x4 ++ ")"

\end{verbatim}
\caption{The Function {\tt showExp}}
\label{showExp-fig}
\end{figure}

\subsection{An Orchestra Example}

Figure \ref{csound-orc-file-fig} shows a typical CSound orchestra
file.  Figure \ref{orc-def-fig} shows how this same functionality
would be achieved in Haskore using an {\tt Orchestra} value.  Finally,
Figure \ref{orc-file-result-fig} shows the result of applying
{\tt writeOrchestra} to {\tt orc1} shown in Figure \ref{orc-def-fig}.
Figures \ref{csound-orc-file-fig} and \ref{orc-file-result-fig}
should be compared: you will note that except for name changes, they
are the same, as they should be.

\begin{figure}
\begin{verbatim} 

sr = 48000
kr = 24000
ksmps = 2
nchnls = 2

instr 4

inote = cpspch(p5)

k1 envlpx ampdb(p4), .001, p3, .05, 6, -.1, .01
k2 envlpx ampdb(p4), .0005, .1, .1, 6, -.05, .01
k3 envlpx ampdb(p4), .001, p3, p3, 6, -.3, .01

a1 oscili k1, inote, 1
a2 oscili k1, inote * 1.004, 1
a3 oscili k2, inote * 16, 1
a4 oscili k3, inote, 5
a5 oscili k3, inote * 1.004, 5

outs  (a2 + a3 + a4) * .75, (a1 + a3 + a5) * .75

endin

\end{verbatim} 
\caption{Sample CSound Orchestra File}
\label{csound-orc-file-fig}
\end{figure}

\begin{figure}
\begin{verbatim} 

> orc1 :: Orchestra
> orc1 = 
>   let hdr   = (48000, 24000, 2)
>       inote = PchToHz p5
>       k1    = Env  CR (DbToAmp p4) 0.001  p3  0.05 6 (-0.1)  0.01 0
>       k2    = Env  CR (DbToAmp p4) 0.0005 0.1 0.1  6 (-0.05) 0.01 0
>       k3    = Env  CR (DbToAmp p4) 0.001  p3  p3   6 (-0.3)  0.01 0
>       a1    = OscI AR k1  inote        1
>       a2    = OscI AR k1 (inote*1.004) 1
>       a3    = OscI AR k2 (inote*16)    1
>       a4    = OscI AR k3  inote        5
>       a5    = OscI AR k3 (inote*1.004) 5
>       out   = StereoOut ((a2+a3+a4) * 0.75) ((a1+a3+a5) * 0.75)
>       ib    = (4,out)
>   in (hdr,[ib]) 

> t1 = processOrcExp (snd (head (snd orc1)))

\end{verbatim} 
\caption{Haskore Orchestra Definition}
\label{orc-def-fig}
\end{figure}

\begin{figure}
\begin{verbatim} 

sr     = 48000
kr     = 24000
ksmps  = 2.0
nchnls = 2

instr 4
k1 envlpx ampdb(p4), 0.001, p3, p3, 6.0, -0.3, 0.01, 0.0
a2 osci k1, (cpspch(p5) * 1.004), 5.0
k3 envlpx ampdb(p4), 0.0005, 0.1, 0.1, 6.0, -0.05, 0.01, 0.0
a4 osci k3, (cpspch(p5) * 16.0), 1.0
k5 envlpx ampdb(p4), 0.001, p3, 0.05, 6.0, -0.1, 0.01, 0.0
a6 osci k5, cpspch(p5), 1.0
a7 osci k1, cpspch(p5), 5.0
a8 osci k5, (cpspch(p5) * 1.004), 1.0
outs (((a8 + a4) + a7) * 0.75), (((a6 + a4) + a2) * 0.75)
endin

\end{verbatim} 
\caption{Result of {\tt writeOrchestra orc1}}
\label{orc-file-result-fig}
\end{figure}

--------------------------------------------------------------------


