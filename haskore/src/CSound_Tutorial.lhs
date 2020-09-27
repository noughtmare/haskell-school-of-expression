\section{Haskore CSound Tutorial}
\label{csound-tut}

\begin{verbatim}

> module CSound_Tutorial where
>
> import CSound
> import System.IO
> import System.Process ( system )
> import System.Info ( os )

\end{vebatim}

	This brief tutorial is designed to introduce the user to the 
capabilities of the CSound software synthesizer and sound synthesis in
general.

\subsection{Additive Synthesis}
\label{add-syn-sect}

	The first part of the tutorial introduces {\em additive synthesis}. 
Additive synthesis is the most basic, yet the most powerful synthesis 
technique available, giving complete control over the sound waveform. 
The basic premiss behind additive sound synthesis is quite simple - defining
a complex sound by specifying each contributing sine wave. The computer is 
very good at generating pure tones, but these are not very interesting. 
However, any sound imaginable can be reproduced as a sum of pure tones. We
can define an instrument of pure tones easily in Haskore. First we define 
a {\em function table} containing a lone sine wave. We can do this using 
the {\tt simpleSine} function defined in the CSound module:
\begin{verbatim}

> pureToneTN :: Int
> pureToneTN = 1
> pureTone = CSTable pureToneTN 0 8192 True (compSine1 [1.0])

\end{verbatim}
	{\tt pureToneTN} is the table number of the simple sine wave. We will
adopt the convention in this tutorial that variables ending with {\tt TN} 
represent table numbers.
	Recall that {\tt compSine1} is defined in the module {\tt CSound} as a
sine wave generating routine {/tt (GEN10)}. In order to have a complete 
score file, we also need a tune. Here is a simple example:
\begin{verbatim}

> tune1 :: Music
>
> tune1 = let v = Volume 150
>        in  c 8 hn [v] :+: e 8 hn [v] :+: g 8 hn [v] :+: 
>            c 9 hn [v] :+: a 8 hn [v] :+: c 9 qn [v] :+: 
>            a 8 qn [v] :+: g 8 dhn [v]:+: qnr

\end{verbatim}
	Recall that the tune, a value of type {\tt Music}, must first be 
converted to a value of type {\tt Performance} using the function 
{\tt perform} defined in the module {\tt Basics}, and then the 
{\tt Performance} must turned into a {\tt Score}, using {\tt perfToScore}
defined in the {\tt CSound} module. Since the function {\tt perform} expects
to see a {\em name map}, we must create one. We won't be using more than two
simple instruments in this tutorial:
\begin{verbatim}

> instNames :: NameMap
> instNames = [("inst1", 1), ("inst2", 2)]
>
> inst1, inst2 :: Int
> inst1 = 1
> inst2 = 2

\end{verbatim}
We can now create a complete Score as follows:
\begin{verbatim}

> defPMap :: PName -> Player
> defPMap _ = defPlayer
> 
> defCon = Context 0 defPlayer "" 1.0 0 0
>
> scored m = perfToScore instNames (perform defPMap defCon m)
>
> score1 = pureTone : scored (Instr "inst1" tune1) 

\end{verbatim}
Let's define an instrument in the orchestra file that will use the function
table {\tt pureTone}:
\begin{verbatim}

> oe1 :: OrcExp
> oe1 = let signal = Osc AR (DbToAmp noteVol) (PchToHz notePit) 
>                           (Const (float pureToneTN))  
>       in  StereoOut signal signal

\end{verbatim}
	This instrument will simply oscillate through the function table
containing the sine wave at the appropriate frequency given by 
{\tt notePit}, and the resulting sound will have an amplitude given by 
{\tt noteVol}. 
Note that the {\tt oe1} expression above is an {\tt OrcExp}, not a complete
{\tt Orchestra}. We need to define a {\em header} and associate {\tt oe1} 
with the instrument that's playing it:
\begin{verbatim}

> hdr :: (Int, Int, Int)
> hdr = (44100, 4410, 2)
>
> o1 = let i = (inst1, oe1)
>      in  (hdr, [i])

\end{verbatim}
	The header above indicates that the audio signals are generated at 
44,100 Hz (CD quality), the control signals are generated at 4,410 Hz, and
there are 2 output channels for stereo sound.
	Now we have a complete score and orchestra that can be converted to a
sound file by CSound and played as follows:
\begin{verbatim}

> csoundDir :: Name
> csoundDir = "C:/TEMP/csound"
> 
> tut1 = playCS o1 score1

\end{verbatim} 
	If you listen to the tune, you will notice that it sounds very thin
and uninteresting. Most musical sounds are not pure. Instead they usually
contain a sine wave of dominant frequency, called a {\em fundamental}, and
a number of other sine waves called {\em partials}. Partials with 
frequencies that are integer multiples of the fundamental are called 
{\em harmonics}. In musical terms, the first harmonic lies an octave above
the fundamental, second harmonic a fifth above the first one, the third 
harmonic lies a major third above the second harmonic etc. This is the 
familiar {\em overtone series}. We can add harmonics to our sine wave 
instrument easily using the {\tt compSine} function defined in the 
{\tt Csound} module. The function takes a list of harmonic strengths as 
arguments. The following creates a function table containing the 
fundamental and the first two harmonics at two thirds and one third of the
strength of the fundamental:
\begin{verbatim}

> twoHarmsTN :: Int
> twoHarmsTN = 2
> twoHarms = CSTable twoHarmsTN 0 8192 True (compSine1 [1.0, 0.66, 0.33])

\end{verbatim}
We can again proceed to create complete score and orchestra files as above:
\begin{verbatim}

> score2 = twoHarms : scored (Instr "inst1" tune1)
>
> oe2 :: OrcExp
> oe2 = let signal = Osc AR (DbToAmp noteVol) (PchToHz notePit) 
>                           (Const (float twoHarmsTN)) 
>       in  StereoOut signal signal
>
> o2 = let i = (inst1, oe2)
>      in  (hdr, [i])
>
> tut2 = playCS o2 score2

\end{verbatim}
	The orchestra file is the same as before - a single oscillator scanning
a function table at a given frequency and volume. This time, however, the 
tune will not sound as thin as before since the table now contains a 
function that is an addition of three sine waves. (Note that the same effect
could be achieved using a simple sine wave table and three oscillators).
	Not all musical sounds contain harmonic partials exclusively, and 
never do we encounter instruments with static amplitude envelope like the 
ones we have seen so far. Most sounds, musical or not, evolve and change
throughout their duration. Let's define an instrument containing both 
harmonic and nonharmonic partials, that starts at maximum amplitude with a
straight line decay. We will use the function {\tt compSine2} from the
{\tt CSound} module to create the function table. {\tt compSine2} takes a 
list of triples as an argument. The triples specify the partial number as
a multiple of the fundamental, relative partial strength, and initial phase
offset:
\begin{verbatim}

> manySinesTN :: Int
> manySinesTN = 3
> manySines = CSTable manySinesTN 0 8192 True (compSine2 [(0.5, 0.9, 0.0),
>                     (1.0, 1.0, 0.0), (1.1, 0.7, 0.0), (2.0, 0.6, 0.0),
>                     (2.5, 0.3, 0.0), (3.0, 0.33, 0.0), (5.0, 0.2, 0.0)])

\end{verbatim}
	Thus this complex will contain the second, third, and fifth harmonic,
nonharmonic partials at frequencies of 1.1 and 2.5 times the fundamental, 
and a component at half the frequency of the fundamental. Their strengths 
relative to the fundamental are given by the second argument, and they all 
start in sync with zero offset.
	Now we can proceed as before to create score and orchestra files. We
will define an {\em amplitude envelope} to apply to each note as we 
oscillate through the table. The amplitude envelope will be a straight line
signal ramping from 1.0 to 0.0 over the duration of the note. This signal 
will be generated at {\em control rate} rather than audio rate, because the
control rate is more than sufficient (the audio signal will change volume 
4,410 times a second), and the slower rate will improve performance.
\begin{verbatim}

> score3 = manySines : scored (Instr "inst1" tune1) 
>
> oe3 :: OrcExp
> oe3 = let ampenv = Line CR 1.0 noteDur 0.0
>           signal = Osc AR (ampenv * (DbToAmp noteVol)) (PchToHz notePit) 
>                           (Const (float manySinesTN))
>       in  StereoOut signal signal
>
> o3 = let i = (inst1, oe3)
>      in  (hdr, [i])
>
> tut3 = playCS o3 score3

\end{verbatim}
	Not only do musical sounds usually evolve in terms of overall 
amplitude, they also evolve their {\em spectra}. In other words, the 
contributing partials do not usually all have the same amplitude envelope,
and so their contribution to the overall sound isn't static. Let us 
illustrate the point using the same set of partials as in the above example.
Instead of creating a table containing a complex waveform, however, we will
use multiple oscillators going through the simple sine wave table we created
at the beginning of this tutorial at the appropriate frequencies. Thus 
instead of the partials being fused together, each can have its own 
amplitude envelope, making the sound evolve over time. The score will be 
score1, defined above.
\begin{verbatim}

> oe4 :: OrcExp
> oe4 = let pitch      = PchToHz notePit
>           amp        = DbToAmp noteVol
>           mkLine t   = LineSeg CR 0 (noteDur*t) 1 [((noteDur* (1-t)), 0)]
>           aenv1      = Line CR 1 noteDur 0 
>           aenv2      = mkLine 0.17 
>           aenv3      = mkLine 0.33
>           aenv4      = mkLine 0.50
>           aenv5      = mkLine 0.67
>           aenv6      = mkLine 0.83
>           aenv7      = Line CR 0 noteDur 1
>           mkOsc ae p = Osc AR (ae * amp) (pitch * p) 
>                               (Const (float pureToneTN))
>           a1         = mkOsc aenv1 0.5
>           a2         = mkOsc aenv2 1.0 
>           a3         = mkOsc aenv3 1.1
>           a4         = mkOsc aenv4 2.0
>           a5         = mkOsc aenv5 2.5
>           a6         = mkOsc aenv6 3.0
>           a7         = mkOsc aenv7 5.0
>           out        = 0.5 * (a1 + a2 + a3 + a4 + a5 + a6 + a7)
>       in  StereoOut out out
>
> o4 = let i = (inst1, oe4)
>      in  (hdr, [i]) 
> 
> tut4 = playCS o4 score1

\end{verbatim}
	So far, we have only used function tables to generate audio signals,
but they can come very handy in {\em modifying} signals. Let us create a 
function table that we can use as an amplitude envelope to make our 
instrument more interesting. The envelope will contain an immediate sharp
attack and decay, and then a second, more gradual one, so we'll have two
attack/decay events per note. We'll use the cubic spline curve generating 
routine to do this:
\begin{verbatim}

> coolEnvTN :: Int
> coolEnvTN = 4
> coolEnv = CSTable coolEnvTN 0 8192 True (cubicSpline 1 [(1692, 0.2),
>                                           (3000, 1), (3500, 0)])

\end{verbatim}
	Let us also add some {\em pfields} to the notes in our score. The two
pfields we add will be used for {\em panning} - the first one will be the
starting percentage of the left channel, the second one the ending 
percentage (1 means all left, 0 all right, 0.5 middle. Pfields of 1 and 0
will cause the note to pan completely from left to right for example)
\begin{verbatim}

> tune2 = let v = Volume 140
>         in  c 8 hn [v, PFields [1.0, 0.75]] :+: 
>             e 8 hn [v, PFields [0.75, 0.5]] :+: 
>             g 8 hn [v, PFields [0.5, 0.25]] :+: 
>             c 9 hn [v, PFields [0.25, 0.0]] :+: 
>             a 8 hn [v, PFields [0.0 ,1.0]]  :+: 
>             c 9 qn [v, PFields [0.0, 0.0]]  :+: 
>             a 8 qn [v, PFields [1.0, 1.0]]  :+: 
>            (g 8 dhn [v, PFields [1.0, 0.0]] :=:
>             g 8 dhn [v, PFields [0.0, 1.0]]):+: qnr

\end{verbatim}
	So far we have limited ourselves to using only sine waves for our 
audio output, even though Csound places no such restrictions on us. Any
repeating waveform, of any shape, can be used to produce pitched sounds. 
In essence, when we are adding sinewaves, we are changing the shape of the
wave. For example, adding odd harmonics to a fundamental at strengths equal
to the inverse of their partial number (ie. third harmonic would be 1/3 the
strength of the fundamental, fifth harmonic 1/5 the fundamental etc) would
produce a {\em square} wave which has a raspy sound to it. Another common
waveform is the {\em sawtooth}, and the more mellow sounding {\em triangle}.
The {\tt CSound} module already contains functions to create these common 
waveforms. Let's use them to create tables that we can use in an instrument:
\begin{verbatim}

> triangleTN, squareTN, sawtoothTN :: Int
> triangleTN = 5
> squareTN   = 6
> sawtoothTN = 7
> triangleT = triangle triangleTN
> squareT   = square   squareTN
> sawtoothT = sawtooth sawtoothTN
>
> score4 = squareT : triangleT : sawtoothT : coolEnv : 
>                    scored (Tempo 0.5 (Instr "inst1" tune2))
>
> oe5 :: OrcExp
> oe5 = let pitch  = PchToHz notePit
>           amp    = DbToAmp noteVol
>           pan    = Line CR p1 noteDur p2
>           oscF   = 1 / noteDur
>           ampen  = Osc CR amp oscF (Const (float coolEnvTN))
>           signal = Osc AR ampen pitch (Const (float squareTN))
>           left   = signal * pan 
>           right  = signal * (1-pan)
>       in  StereoOut left right
>
> o5 = let i = (inst1, oe5)
>      in  (hdr, [i]) 
>
> tut5 = playCS o5 score4

\end{verbatim}
	This will oscillate through a table containing the square wave. 
Check out the other waveforms too and see what they sound like. This can be
done by specifying the table to be used in the orchestra file.
	As our last example of additive synthesis, we will introduce an 
orchestra with multiple instruments. The bass will be mostly in the left 
channel, and will be the same as the third example instrument in this 
section. It will play the tune two octaves below the instrument in the right
channel, using an orchestra identical to {\tt oe3} with the addition of the 
panning feature:
\begin{verbatim}

> score5 = manySines : pureTone : scored (Instr "inst1" tune1) ++ 
>                                 scored (Instr "inst2" tune1)
>
> oe6 :: OrcExp
> oe6 = let ampenv = Line CR 1.0 noteDur 0.0
>           signal = Osc AR (ampenv * (DbToAmp noteVol))
>                        (PchToHz (notePit - 2)) (Const (float manySinesTN))
>           left   = 0.8 * signal
>           right  = 0.2 * signal
>       in  StereoOut left right
>
> oe7 :: OrcExp
> oe7 = let pitch      = PchToHz notePit
>           amp        = DbToAmp noteVol
>           mkLine t   = LineSeg CR 0 (noteDur*t) 0.5 [((noteDur* (1-t)), 0)]
>           aenv1      = Line CR 0.5 noteDur 0 
>           aenv2      = mkLine 0.17 
>           aenv3      = mkLine 0.33
>           aenv4      = mkLine 0.50
>           aenv5      = mkLine 0.67
>           aenv6      = mkLine 0.83
>           aenv7      = Line CR 0 noteDur 0.5
>           mkOsc ae p = Osc AR (ae * amp) (pitch * p) 
>                               (Const (float pureToneTN))
>           a1         = mkOsc aenv1 0.5
>           a2         = mkOsc aenv2 1.0 
>           a3         = mkOsc aenv3 1.1
>           a4         = mkOsc aenv4 2.0
>           a5         = mkOsc aenv5 2.5
>           a6         = mkOsc aenv6 3.0
>           a7         = mkOsc aenv7 5.0
>           left       = 0.2 * (a1 + a2 + a3 + a4 + a5 + a6 + a7)
>           right      = 0.8 * (a1 + a2 + a3 + a4 + a5 + a6 + a7)
>       in  StereoOut left right
>
> o6 = let i1 = (inst1, oe6)
>          i2 = (inst2, oe7)
>      in  (hdr, [i1, i2]) 
>
> tut6 = playCS o6 score5

\end{verbatim}
	Additive synthesis is the most powerful tool in computer music and 
sound synthesis in general. It can be used to create any sound imaginable,
whether completely synthetic or a simulation of a real-world sound, and 
everyone interested in using the computer to synthesize sound should be well
versed in it. The most significant drawback of additive synthesis is that it
requires huge amounts of control data, and potentially thousands of 
oscillators. There are other synthesis techniques, such as 
{\em modulation synthesis}, that can be used to create rich and interesting
timbres at a fraction of the cost of additive synthesis, though no other 
synthesis technique provides quite the same degree of control.

\subsection{Modulation Synthesis}
\label{mod-syn-sect}

	While additive synthesis provides full control and great flexibility,
it is quiet clear that the enormous amounts of control data make it 
impractical for even moderately complicated sounds. There is a class of 
synthesis techniques that use {\em modulation} to produce rich, time-varying
timbres at a fraction of the storage and time cost of additive synthesis. 
The basic idea behind modulation synthesis is controlling the 
amplitude and/or frequency of the main periodic signal, called the 
{\em carrier}, by another periodic signal, called the {\em modulator}.
The two main kinds of modulation synthesis are {\em amplitude modulation} 
and {\em frequency modulation} synthesis. Let's start our discussion with
the simpler one of the two - amplitude synthesis.
	We have already shown how to supply a time varying amplitude envelope 
to an oscillator. What would happen if this amplitude envelope was itself 
an oscillating signal? Supplying a low frequency (<20Hz) modulating signal
would create a predictable effect - we would hear the volume of the carrier
signal go periodically up and down. However, as the modulator moves into the
audible frequency range, the carrier changes timbre as new frequencies 
appear in the spectrum. The new frequencies are equal to the sum and 
difference of the carrier and modulator. So for example, if the frequency of
the main signal (carrier) is C = 500Hz, and the frequency of the modulator 
is M = 100Hz, the audible frequencies will be the carrier C (500Hz), 
C + M (600Hz), and  C - M (400Hz). The amplitude of the two new sidebands 
depends on the amplitude of the modulator, but will never exceed half the 
amplitude of the carrier.
	The following is a simple example that demonstrates amplitude 
modulation. The carrier will be a 10 second pure tone at 500Hz. The 
frequency of the modulator will increase linearly over the 10 second 
duration of the tone from 0 to 200 Hz. Initially, you will be able to hear 
the volume of the signal fluctuate, but after a couple of seconds the volume
will seem constant as new frequencies appear.	
	Let us first create the score file. It will contain a sine wave table,
and a single note event:
\begin{verbatim}

> score6 = pureTone : [ CSNote 1 0.0 10.0 500.0 10000.0 [] ]

\end{verbatim}
	The orchestra will contain a single AM instrument. The carrier will 
simply oscillate through the sine wave table at frequency given by the note
pitch (500Hz, see the score above), and amplitude given by the modulator. 
The modulator will oscillate through the same sine wave table at frequency 
ramping from 0 to 200Hz. The modulator should be a periodic signal that 
varies from 0 to the maximum volume of the carrier. Since the sine wave goes
from -1 to 1, we will need to add 1 to it and half it, before multiplying it
by the volume supplied by the note event. This will be the modulating 
signal, and the carrier's amplitude input. (note that we omit the conversion
functions DbToAmp and notePit, since we supply the amplitude and frequency 
in their raw units in the score file)
\begin{verbatim}

> oe8 :: OrcExp
> oe8 = let modfreq = Line CR 0.0 noteDur 200.0
>           modamp  = Osc AR 1.0 modfreq (Const (float pureToneTN))
>           modsig  = (modamp + 1.0) * 0.5 * noteVol
>           carrier = Osc AR modsig notePit (Const (float pureToneTN))
>       in  StereoOut carrier carrier
>
> o7 = let i = (inst1, oe8)
>      in  (hdr, [i])
>
> tut7 = playCS o7 score6

\end{verbatim}
	Next synthesis technique on the palette is {\em frequency modulation.}
As the name suggests, we modulate the frequency of the carrier. Frequency 
modulation is much more powerful and interesting than amplitude modulation,
because instead of getting two sidebands, FM gives a {\em number} of 
spectral sidebands. Let us begin with an example of a simple FM. We will 
again use a single 10 second note and a 500Hz carrier. Remember that when we
talked about amplitude modulation, the amplitude of the sidebands was 
dependent upon the amplitude of the modulator. In FM, the modulator 
amplitude plays a much bigger role, as we will see soon. To negate the 
effect of the modulator amplitude, we will keep the ratio of the modulator
amplitude and frequency constant at 1.0 (we will explain shortly why). The
frequency and amplitude of the modulator will ramp from 0 to 200 over the
duration of the note. This time, though, unlike with AM, we will hear a 
whole series of sidebands. The orchestra is just as before, except we 
modulate the frequency instead of amplitude. 
\begin{verbatim}

> oe9 :: OrcExp
> oe9 = let modfreq = Line CR 0.0 noteDur 200.0
>           modamp  = modfreq 
>           modsig  = Osc AR modamp modfreq (Const (float pureToneTN))
>           carrier = Osc AR noteVol (notePit + modsig) 
>                                    (Const (float pureToneTN))
>       in  StereoOut carrier carrier
>
> o8 = let i = (inst1, oe9)
>      in  (hdr, [i])
>
> tut8 = playCS o8 score6

\end{verbatim}
	The sound produced by FM is a little richer but still very bland. Let
us talk now about the role of the {\em depth} of the frequency modulation
(the amplitude of the modulator). Unlike in AM, where we only had one 
spectral band on each side of the carrier frequency (ie we heard C, C+M, 
C-M), FM gives a much richer spectrum with many sidebands. The frequencies 
we hear are C, C+M, C-M, C+2M, C-2M, C+3M, C-3M etc. The amplitudes of the
sidebands are determined by the {\em modulation index} I, which is the ratio
between the amplitude (also referred to as depth) and frequency of the 
modulator (I = D / M). As a rule of thumb, the number of significant 
sideband pairs (at least 1% the volume of the carrier) is I+1. As I (and the
number of sidebands) increases, energy is "stolen" from the carrier and 
distributed among the sidebands. Thus if I=1, we have 2 significant sideband
pairs, and the audible frequencies will be C, C+M, C-M, C+2M, C-2M, with C,
the carrier, being the dominant frequency. When I=5, we will have a much 
richer sound with about 6 significant sideband pairs, some of which will 
actually be louder than the carrier. Let us explore the effect of the 
modulation index in the following example. We will keep the frequency of 
the carrier and the modulator constant at 500Hz and 80 Hz respectively.
The modulation index will be a stepwise function from 1 to 10, holding each 
value for one second. So in effect, during the first second (I = D/M = 1),
the amplitude of the modulator will be the same as its frequency (80). 
During the second second (I = 2), the amplitude will be double the frequency
(160), then it will go to 240, 320, etc:
\begin{verbatim}

> oe10 :: OrcExp
> oe10 = let modind  = LineSeg CR 1 1 1 [(0,2), (1,2), (0,3), (1,3), (0,4), 
>                                        (1,4), (0,5), (1,5), (0,6), (1,6),
>                                        (0,7), (1,7), (0,8), (0,9), (1,9),
>                                        (0,10), (1,10)]
>            modamp  = 80.0 * modind 
>            modsig  = Osc AR modamp 80.0 (Const (float pureToneTN))
>            carrier = Osc AR noteVol (notePit + modsig)
>                                     (Const (float pureToneTN))
>        in  StereoOut carrier carrier
>
> o9 = let i = (inst1, oe10)
>      in  (hdr, [i])
>
> tut9 = playCS o9 score6

\end{verbatim}
	Notice that when the modulation index gets high enough, some of the 
sidebands have negative frequencies. For example, when the modulation index
is 7, there is a sideband present in the sound with a frequency 
C - 7M = 500 - 560 = -60Hz. The negative sidebands get reflected back into 
the audible spectrum but are {\em phase shifted} 180 degrees, so it is an 
inverse sine wave. This makes no difference when the wave is on its own, but
when we add it to its inverse, the two will cancel out. Say we set the 
frequency of the carrier at 100Hz instead of 80Hz. Then at I=6, we would 
have present two sidebands of the same frequency - C-4M = 100Hz, and 
C-6M = -100Hz. When these two are added, they would cancel each other out
(if they were the same amplitude; if not, the louder one would be attenuated
by the amplitude of the softer one). The following flexible instrument will
sum up simple FM. The frequency of the modulator will be determined by the
C/M ratio supplied as p1 in the score file. The modulation index will be a
linear slope going from 0 to p2 over the duration of each note. Let us also
add panning control as in additive synthesis - p3 will be the initial left 
channel percentage, and p4 the final left channel percentage:
\begin{verbatim}

> oe11 :: OrcExp
> oe11 = let carfreq = PchToHz notePit
>            caramp  = DbToAmp noteVol
>            modfreq = carfreq * p1
>            modind  = Line CR 0 noteDur p2
>            modamp  = modfreq * modind 
>            modsig  = Osc AR modamp modfreq (Const (float pureToneTN))
>            carrier = Osc AR caramp (carfreq + modsig)
>                                    (Const (float pureToneTN))
>            mainamp = Osc CR 1.0 (1/noteDur) (Const (float coolEnvTN))
>            pan     = Line CR p3 noteDur p4
>            left    = mainamp * pan * carrier
>            right   = mainamp * (1 - pan) * carrier
>        in  StereoOut left right
>
> o10 = let i = (inst1, oe11)
>       in  (hdr, [i])

\end{verbatim}
	Let's write a cool tune to show off this instrument. Let's keep it 
simple and play the chord progression Em - C - G - D a few times, each time
changing some if the parameters:
\begin{verbatim}

> emchord, cchord, gchord, dchord :: 
>                   Float -> Float -> Float -> Float -> Music
>
> emchord x y z w = let v = Volume 140
>                   in  (e 6 wn [v, PFields [x, y, z, w]] :=:
>                        g 6 wn [v, PFields [x, y, z, w]] :=:
>                        b 6 wn [v, PFields [x, y, z, w]])
>
> cchord x y z w  = let v = Volume 140
>                   in  (c 6 wn [v, PFields [x, y, z, w]] :=:
>                        e 6 wn [v, PFields [x, y, z, w]] :=:
>                        g 6 wn [v, PFields [x, y, z, w]])
>
> gchord x y z w  = let v = Volume 140
>                   in  (g 6 wn [v, PFields [x, y, z, w]] :=:
>                        b 6 wn [v, PFields [x, y, z, w]] :=:
>                        d 7 wn [v, PFields [x, y, z, w]])
>
> dchord x y z w  = let v = Volume 140
>                   in  (d 6 wn [v, PFields [x, y, z, w]] :=:
>                        fs 6 wn [v, PFields [x, y, z, w]] :=:
>                        a 6 wn [v, PFields [x, y, z, w]])
>
> tune3 :: Music
> tune3 = (emchord 3.0 2.0 0.0 1.0) :+: (cchord 3.0 5.0 1.0 0.0) :+:
>         (gchord  3.0 8.0 0.0 1.0) :+: (dchord 3.0 12.0 1.0 0.0) :+:
>         (emchord 3.0 4.0 0.0 0.5) :+: (cchord 5.0 4.0 0.5 1.0) :+:
>         (gchord  8.0 4.0 1.0 0.5) :+: (dchord 10.0 4.0 0.5 0.0) :+:
>         ((emchord 4.0 6.0 1.0 0.0) :=: (emchord 7.0 5.0 0.0 1.0)) :+:
>         ((cchord  5.0 9.0 1.0 0.0) :=: (cchord  9.0  5.0 0.0 1.0)) :+:
>         ((gchord  5.0 5.0 1.0 0.0)  :=: (gchord  7.0 7.0 0.0 1.0)) :+:
>         ((dchord  2.0 3.0 1.0 0.0)  :=: (dchord  7.0 15.0 0.0 1.0)) 

\end{verbatim}
	Now we can create a score. It will contain two wave tables - one 
containing the sine wave, and the other containing an amplitude envelope,
which will be the table coolEnv which we have already seen before
\begin{verbatim}

> score7 = pureTone : coolEnv : scored (Tempo 0.5 (Instr "inst1" tune3))
>
> tut10 = playCS o10 score7

\end{verbatim}
	Note that all of the above examples of frequency modulation use a 
single carrier and a single modulator, and both are oscillating through the
simplest of waveforms - a sine wave. Already we have achieved some very rich
and interesting timbres using this simple technique, but the possibilities 
are unlimited when we start using different carrier and modulator waveshapes
and multiple carriers and/or modulators. Let us include a couple more 
examples that will play the same chord progression as above with multiple 
carriers, and then with multiple modulators. 
	The reason for using multiple carriers is to obtain 
{/em formant regions} in the spectrum of the sound. Recall that when we 
modulate a carrier frequency we get a spectrum with a central peak and a 
number of sidebands on either side of it. Multiple carriers introduce 
additional peaks and sidebands into the composite spectrum of the resulting
sound. These extra peaks are called formant regions, and are characteristic
of human voice and most musical instruments
\begin{verbatim}

> oe12 :: OrcExp
> oe12 = let car1freq = PchToHz notePit
>            car2freq = PchToHz (notePit + 1)
>            car1amp  = DbToAmp noteVol
>            car2amp  = DbToAmp noteVol * 0.7
>            modfreq  = car1freq * p1
>            modind   = Line CR 0 noteDur p2
>            modamp   = modfreq * modind 
>            modsig   = Osc AR modamp modfreq (Const (float pureToneTN))
>            carrier1 = Osc AR car1amp (car1freq + modsig) 
>                                      (Const (float pureToneTN))
>            carrier2 = Osc AR car2amp (car2freq + modsig) 
>                                      (Const (float pureToneTN)) 
>            mainamp  = Osc CR 1.0 (1/noteDur) (Const (float coolEnvTN))
>            pan      = Line CR p3 noteDur p4
>            left     = mainamp * pan * (carrier1 + carrier2)
>            right    = mainamp * (1 - pan) * (carrier1 + carrier2)
>        in  StereoOut left right
>
> o11 = let i = (inst1, oe12)
>       in  (hdr, [i])
>
> tut11 = playCS o11 score7

\end{verbatim}
	In the above example, there are two formant regions - one is centered 
around the note pitch frequency provided by the score file, the other an
octave above. Both are modulated in the same way by the same modulator. The
sound is even richer than that obtained by simple FM.
	Let us now turn to multiple modulator FM. In this case, we use a 
signal to modify another signal, and the modified signal will itself become
a modulator acting on the carrier. Thus the wave that wil be modulating the
carrier is not a sine wave as above, but is itself a complex waveform 
resulting from simple FM. The spectrum of the sound will contain a central
peak frequency, surrounded by a number of sidebands, but this time each 
sideband will itself also by surrounded by a number of sidebands of its own.
So in effect we are talking about "double" modulation, where each sideband
is a central peak in its own little spectrum. Multiple modulator FM thus
provides extremely rich spectra
\begin{verbatim}

> oe13 :: OrcExp
> oe13 = let carfreq  = PchToHz notePit
>            caramp   = DbToAmp noteVol
>            mod1freq = carfreq * p1
>            mod2freq = mod1freq * 2.0
>            modind   = Line CR 0 noteDur p2
>            mod1amp  = mod1freq * modind
>            mod2amp  = mod1amp * 3.0 
>            mod1sig  = Osc AR mod1amp mod1freq (Const (float pureToneTN))
>            mod2sig  = Osc AR mod2amp (mod2freq + mod1sig) 
>                                      (Const (float pureToneTN))
>            carrier  = Osc AR caramp (carfreq + mod2sig) 
>                                      (Const (float pureToneTN)) 
>            mainamp  = Osc CR 1.0 (1/noteDur) (Const (float coolEnvTN))
>            pan      = Line CR p3 noteDur p4
>            left     = mainamp * pan * carrier
>            right    = mainamp * (1 - pan) * carrier
>        in  StereoOut left right
>
> o12 = let i = (inst1, oe13)
>       in  (hdr, [i])
>
> tut12 = playCS o12 score7

\end{verbatim}
	In fact, the spectra produced by multiple modulator FM are so rich and
complicated that even the moderate values used as arguments in our tune 
produce spectra that are saturated and otherworldly. And we did this while 
keeping the ratios of the two modulators frequencies and amplitudes 
constant; introducing dynamics in those ratios would produce even crazier 
results. It is quite amazing that from three simple sine waves, the purest 
of all tones, we can derive an unlimited number of timbres. Modulation 
synthesis is a very powerful tool and understanding how to use it can prove
invaluable. The best way to learn how to use FM effectively is to dabble and
experiment with different ratios, formant regions, dynamic relationships 
betweeen ratios, waveshapes, etc. The possibilities are limitless.

\subsection{Other Capabilities Of CSound}
\label{other-sect}

	In our examples of additive and modulation synthesis we only used a
limited number of functions and routines provided us by CSound, such as
Osc (oscillator), Line and LineSig (line and line segment signal
generators) etc. This tutorial intends to briefly explain the 
functionality of some of the other features of CSound. Remember that the
CSound manual should be the ultimate reference when it comes to using
these functions.
	Let us start with the two functions {\tt Buzz} and {\tt GenBuzz}. 
These functions will produce a set of harmonically related cosines. Thus 
they really implement simple additive synthesis, except that the number of
partials can be varied dynamically through the duration of the note, 
rather than staying fixed as in simple additive synthesis. As an example,
let us perform the tune defined at the very beginning of the tutorial using
an instrument that will play each note by starting off with the fundamental
and 70 harmonics, and ending with simply the sine wave fundamental (note 
that cosine and sine waves sound the same). We will use a straight line 
signal going from 70 to 0 over the duration of each note for the number of 
harmonics. The score used will be score1, and the orchestra will be:
\begin{verbatim}

> oe14 :: OrcExp
> oe14 = let numharms = Line AR 70 noteDur 0
>            signal   = Buzz (DbToAmp noteVol) (PchToHz notePit) 
>                        numharms (Const (float pureToneTN))   
>        in  StereoOut signal signal
>
> o13 = let i = (inst1, oe14)
>       in  (hdr, [i])
>
> tut13 = playCS o13 score1

\end{verbatim}
	Let's invert the line of the harmonics, and instead of going from 70
to 0, make it go from 0 to 70. This will produce an interesting effect 
quite different from the one just heard:
\begin{verbatim}

> oe15 :: OrcExp
> oe15 = let numharms = Line AR 0 noteDur 70
>            signal   = Buzz (DbToAmp noteVol) (PchToHz notePit) 
>                        numharms (Const (float pureToneTN))   
>        in  StereoOut signal signal
>
> o14 = let i = (inst1, oe15)
>       in  (hdr, [i])
>
> tut14 = playCS o14 score1

\end{verbatim}
	The {\tt Buzz} expression takes the overall amplitude, fundamental 
frequency, number of partials, and a sine wave table and generates a 
wave complex.
	In recent years there has been a lot of research conducted in the
area of {\em physical modelling}. This technique attempts to approximate the
sound of real world musical instruments through mathematical models. One 
of the most widespread, versatile and interesting of these models is the
{\em Karplus-Strong algorithm} that simulates the sound of a plucked string.
The algorithm starts off with a buffer containing a user-determined 
waveform. On every pass, the waveform is "smoothed out" and flattened by the
algorithm to simulate the decay. There is a certain degree of randomness 
involved to make the string sound more natural. 
	There are six different "smoothing methods" available in CSound, as 
mentioned in the CSound module. The {\tt Pluck} constructor accepts the note
volume, pitch, the table number that is used to initialize the buffer, the 
smoothing method used, and two parameters that depend on the smoothing 
method. If zero is given as the initializing table number, the buffer starts
off containing a random waveform (white noise). This is the best table when
simulating a string instrument because of the randomness and percussive 
attack it produces when used with this algorithm, but you should experiment
with other waveforms as well. 
	Here is an example of what Pluck sounds like with a white noise buffer
and the simple smoothing method. This method ignores the parameters, which we
set to zero.
\begin{verbatim}

> oe16 :: OrcExp
> oe16 = let signal = Pluck (DbToAmp noteVol) (PchToHz notePit) 0 
>                            simpleSmooth 0 0
>        in  StereoOut signal signal
>
> o15 = let i = (inst1, oe16)
>      in  (hdr, [i])
>
> tut15 = playCS o15 score1

\end{verbatim}
	The second smoothing method is the {\em stretched smooth}, which works
like the simple smooth above, except that the smoothing process is stretched
by a factor determined by the first parameter. The second parameter is 
ignored. The third smoothing method is the {\em snare drum} method. The 
first parameter is the "roughness" parameter, with 0 resulting in a sound 
identical to simple smooth, 0.5 being the perfect snare drum, and 1.0 being
the same as simple smooth again with reversed polarity (like a graph flipped
around the x-axis). The fourth smoothing method is the {\em stretched drum} 
method which combines the roughness and stretch factors - the first parameter 
is the roughness, the second is the stretch. The fifth method is 
{\em weighted average} - it combines the current sample (ie. the current pass 
through the buffer) with the previous one, with their weights being determined
by the parameters. This is a way to add slight reverb to the plucked sound. 
Finally, the last method filters the sound so it doesn't sound as bright. 
The parameters are ignored. You can modify the instrument {\tt oe16} easily
to listen to all these effects by simply replacing the variable 
{\tt simpleSmooth} by {\tt stretchSmooth, simpleDrum, stretchDrum, 
weightedSmooth} or {\tt filterSmooth}.
	Here is another simple instrument example. This combines a snare drum
sound with a stretched plucked string sound. The snare drum as a constant 
amplitude, while we apply an amplitude envelope to the string sound. The 
envelope is a spline curve with a hump in the middle, so both the attack and
decay are gradual. The drum roughness factor is 0.3, so a pitch is still 
discernible (with a factor of 0.5 we would get a snare drum sound with no 
pitch, just a puff of white noise). The drum sound is shifted towards the left
channel, while the string sound is shifted towards the right.
\begin{verbatim}

> midHumpTN :: Int
> midHumpTN = 8
> midHump = CSTable midHumpTN 0 8192 True (cubicSpline 0.0 [(4096, 1.0),
>                                            (4096, 0.0)])
>
> score8 = pureTone : midHump : scored (Instr "inst1" tune1) 
>
> oe17 :: OrcExp
> oe17 = let string = Pluck (DbToAmp noteVol) (PchToHz notePit) 0 
>                            stretchSmooth 1.5 0
>            drum   = Pluck 6000 (PchToHz notePit) 0 simpleDrum 0.3 0
>            env    = Osc CR 1.0 (1 / noteDur) (Const (float midHumpTN))
>            left   = (0.65 * drum) + (0.35 * env * string)
>            right  = (0.35 * drum) + (0.65 * env * string)  
>        in  StereoOut left right
>
> o16 = let i = (inst1, oe17)
>       in  (hdr, [i])
>
> tut16 = playCS o16 score8

\end{verbatim}
	Let us now turn our attention to the effects we can achieve using a 
{\em delay line}. Let's return to a simple instrument we defined at the 
beginning of the tutorial - {\tt oe3} specifes an instrument containing both 
harmonic and inharmonic partials, with a linearly decaying amplitude envelope.
Here we take that instrument and add a little echo to it using delay: 
\begin{verbatim}

> oe18 :: OrcExp
> oe18 = let ampenv = Line CR 1.0 noteDur 0.0
>            sig    = Osc AR (ampenv * (DbToAmp noteVol)) (PchToHz notePit) 
>                            (Const (float manySinesTN))
>            dline  = Delay 0.1 sig
>            dsig1  = DelTap 0.05 dline
>            dsig2  = DelTap 0.1  dline
>            left   = (0.65 * sig) + (0.35 * dsig2) + (0.5 * dsig1)
>            right  = (0.35 * sig) + (0.65 * dsig2) + (0.5 * dsig1)
>        in  StereoOut left right
>
> o17 = let i = (inst1, oe18)
>      in  (hdr, [i])
>
> tut17 = playCS o17 score3

\end{verbatim}
	The constructor {\tt Delay} establishes a {\em delay line}. A delay 
line is essentially a buffer that contains the signal to be delayed. The first
argument to the {\tt Delay} constructor  is the length of the delay (which
determines the size of the buffer), and the second argument is the signal to
be delayed. So for example, if the delay time is 1.0 seconds, and the sampling
rate is 44,100 Hz (CD quality), then the delay line will be a buffer containing
44,100 samples of the delayed signal. The buffer is rewritten at the audio 
rate. Once {\tt Delay t sig} writes t seconds of the signal {\tt sig} into the
buffer, the buffer can be {\em tapped} using the {\tt DelTap} or the
{\tt DelTapI} constructors. {\tt DelTap t dline} will extract the signal from 
{\tt dline} at time {\tt t} seconds. In the exmaple above, we set up a delay 
line containing 0.1 seconds of the audio signal, then we tapped it twice - once
at 0.05 seconds and once at 0.1 seconds. The output signal is a combination of
the original signal (left channel), the signal delayed by 0.05 seconds 
(middle), and the signal delayed by 0.1 seconds (right channel). 
	CSound provides other ways to reverberate a signal besides the delay
line just demonstrated. One such way is achieved via the Reverb constructor
introduced in the {\tt CSound} module. This constructor tries to emulate 
natural room reverb, and takes as arguments the signal to be reverberated, and
the reverb time in seconds. This is the time it takes the signal to decay to
1/1000 its original amplitude. In this example we output both the original and
the reverberated sound.
\begin{verbatim}

> oe19 :: OrcExp
> oe19 = let ampenv = Line CR 1.0 noteDur 0.0
>            sig    = Osc AR (ampenv * (DbToAmp noteVol)) (PchToHz notePit) 
>                            (Const (float manySinesTN))
>            rev    = Reverb sig 0.15 
>            left   = (0.65 * sig) + (0.35 * rev)
>            right  = (0.35 * sig) + (0.65 * rev)
>        in  StereoOut left right
>
> o18 = let i = (inst1, oe19)
>      in  (hdr, [i])
>
> tut18 = playCS o18 score5
 
\end{verbatim}
	The other two reverb constructors are {\tt Comb} and {\tt Alpass}. Each
of these requires as arguments the signal to be reverberated, the reverb time
as above, and echo loop density in seconds. Here is an example of an instrument
using {/tt Comb}.
\begin{verbatim}

> oe20 :: OrcExp
> oe20 = let ampenv = Line CR 1.0 noteDur 0.0
>            sig    = Osc AR (ampenv * (DbToAmp noteVol)) (PchToHz notePit) 
>                            (Const (float manySinesTN))
>            rev    = Comb sig 1.0 0.1
>        in  StereoOut rev rev
>
> o19 = let i = (inst1, oe20)
>      in  (hdr, [i])
>
> tut19 = playCS o19 score3

\end{verbatim}
	Delay lines can be used for effects other than simple echo and 
reverberation. Once the delay line has been established, it can be tapped at
times that very at control or audio rates. This can be taken advantage of to
produce effects like chorus, flanger, or the Doppler effect. Here is an
example of the flanger effect. This instrument adds a slight flange to 
{\tt oe11}. 
\begin{verbatim}

> oe21 :: OrcExp
> oe21 = let carfreq = PchToHz notePit
>            ampenv  = Osc CR 1.0 (1/noteDur) (Const (float coolEnvTN))
>            caramp  = (DbToAmp noteVol) * ampenv
>            modfreq = carfreq * p1
>            modind  = Line CR 0 noteDur p2
>            modamp  = modfreq * modind 
>            modsig  = Osc AR modamp modfreq (Const (float pureToneTN))
>            carrier = Osc AR caramp (carfreq + modsig)
>                                    (Const (float pureToneTN))
>            dline   = Delay 1.0 carrier
>            ftime   = Osc AR 0.01 2 (Const (float pureToneTN))
>            flange  = DelTapI (0.5 + ftime) dline
>            flanger = ampenv * flange
>            signal  = carrier + flanger
>            pan     = Line CR p3 noteDur p4
>            left    = pan * signal
>            right   = (1 - pan) * signal
>        in  StereoOut left right
>
> o20 = let i = (inst1, oe21)
>       in  (hdr, [i])
>
> tut20 = playCS o20 score7

\end{verbatim}
	This completes our discussion of sound synthesis and Csound. For more
information, please consult the CSound manual or check out 

http://mitpress.mit.edu/e-books/csound/frontpage.html

> playCS :: Orchestra -> Score -> IO ()
> playCS = play (if os == "mingw32"
>                  then \o -> \s -> "csound -d -o dac " ++ o ++ " " ++ s
>                  else \o -> \s -> "csound --au -d -o stdout -s " ++ o ++
>                                   " " ++ s ++ " | play -t au -")

> play :: (String -> String -> String) -> Orchestra -> Score -> IO ()
> play cmd orc sco =
>     do savefile writeOrc "test.orc" orc
>        savefile printScore "test.sco" sco
>        system (cmd "test.orc" "test.sco")
>        return ()

> savefile write fname s = do
>       h <- openBinaryFile fname WriteMode
>       write h s
>       hClose h


=========================================================================
=========================================================================
=========================================================================

	Here are some bonus instruments for your pleasure and enjoyment.
The first ten instruments are lifted from

http://wings.buffalo.edu/academic/department/AandL/music/pub/accci/01/01_01_1b.txt.html

	The tutorial explains how to add echo/reverb and other effects to the
instruments if you need to. This instrument sounds like an electric piano and
is really simple - pianoEnv sets the amplitude envelope, and the sound 
waveform is just a series of 10 harmonics. To make the sound brighter, 
increase the weight of the upper harmonics. 

> pianoEnvTN, pianoWaveTN :: Int
> pianoEnvTN  = 10
> pianoWaveTN = 11
> pianoEnv    = CSTable pianoEnvTN 0 1024 True (lineSeg1 0 [(20, 0.99), 
>                                       (380, 0.4), (400, 0.2), (224, 0)])
> pianoWave   = CSTable pianoWaveTN  0 1024 True (compSine1 [0.158, 0.316, 
>                       1.0, 1.0, 0.282, 0.112, 0.063, 0.079, 0.126, 0.071])
>
> pianoScore = pianoEnv : pianoWave : scored (Instr "inst1" tune1)
>
> pianoOE :: OrcExp
> pianoOE = let ampenv = Osc CR (DbToAmp noteVol) (1/noteDur) 
>                               (Const (float pianoEnvTN))
>               signal = Osc AR ampenv (PchToHz notePit) 
>                                       (Const (float pianoWaveTN))
>           in  StereoOut signal signal
>
> pianoOrc = let i = (inst1, pianoOE)
>            in  (hdr, [i])
>
> piano = playCS pianoOrc pianoScore

Here is another instrument with a reedy sound to it

> reedyEnvTN, reedyWaveTN :: Int
> reedyEnvTN  = 12
> reedyWaveTN = 13
> reedyEnv    = CSTable reedyEnvTN   0 1024 True (lineSeg1 0 [(172, 1.0),  
>      (170, 0.8), (170, 0.6), (170, 0.7), (170, 0.6), (172,0)])
> reedyWave   = CSTable reedyWaveTN  0 1024 True (compSine1 [0.4, 0.3, 
>                       0.35, 0.5, 0.1, 0.2, 0.15, 0.0, 0.02, 0.05, 0.03])
>
> reedyScore = reedyEnv : reedyWave : scored (Instr "inst1" tune1)
>
> reedyOE :: OrcExp
> reedyOE = let ampenv = Osc CR (DbToAmp noteVol) (1/noteDur) 
>                               (Const (float reedyEnvTN))
>               signal = Osc AR ampenv (PchToHz notePit) 
>                                       (Const (float reedyWaveTN))
>           in  StereoOut signal signal
>
> reedyOrc = let i = (inst1, reedyOE)
>            in  (hdr, [i])
>
> reedy = playCS reedyOrc reedyScore

We can use a little trick to make it sound like several reeds playing by 
adding three signals that are slightly out of tune:

> reedy2OE :: OrcExp
> reedy2OE = let ampenv = Osc CR (DbToAmp noteVol) (1/noteDur) 
>                                (Const (float reedyEnvTN))
>                freq   = PchToHz notePit
>                a1     = Osc AR ampenv freq (Const (float reedyWaveTN))
>                a2     = Osc AR (ampenv * 0.44) (freq + (0.023 * freq))
>                                (Const (float reedyWaveTN))
>                a3     = Osc AR (ampenv * 0.26) (freq + (0.019 * freq))
>                                (Const (float reedyWaveTN))
>                left   = (a1 * 0.5) + (a2 * 0.35) + (a3 * 0.65)
>                right  = (a1 * 0.5) + (a2 * 0.65) + (a3 * 0.35) 
>            in  StereoOut left right
>
> reedy2Orc = let i = (inst1, reedy2OE)
>             in  (hdr, [i])
>
> reedy2 = playCS reedy2Orc reedyScore

This instrument tries to emulate a flute sound by introducing random 
variations to the amplitude envelope. The score file passes in two 
parameters - the first one is the depth of the random tremolo in percent of 
total amplitude. The tremolo is implemented using the RandomI constructor,
which generates a signal that interpolates between 2 random numbers over a
certain number of samples that is specified by teh second parameter.

> fluteTune :: Music
>
> fluteTune = let v = Volume 160
>                 p = PFields [30, 40]        
>             in  c 8 hn [v, p] :+: e 8 hn [v, p] :+: g 8 hn [v, p] :+: 
>                 c 9 hn [v, p] :+: a 8 hn [v, p] :+: c 9 qn [v, p] :+: 
>                 a 8 qn [v, p] :+: g 8 dhn [v, p]:+: qnr
>
> fluteEnvTN, fluteWaveTN :: Int
> fluteEnvTN  = 14
> fluteWaveTN = 15
> fluteEnv    = CSTable fluteEnvTN   0 1024 True (lineSeg1 0 [(100, 0.8),  
>                         (200, 0.9), (100, 0.7), (300, 0.2), (324, 0.0)])
> fluteWave   = CSTable fluteWaveTN  0 1024 True (compSine1 [1.0, 0.4, 
>                                                  0.2, 0.1, 0.1, 0.05])
>
> fluteScore = fluteEnv : fluteWave : scored (Instr "inst1" fluteTune)
>
> fluteOE :: OrcExp
> fluteOE = let vol    = DbToAmp noteVol
>               rand   = RandomI AR ((vol/100) * p1) p2
>               ampenv = OscI AR (rand + vol) (1 / noteDur) 
>                                (Const (float fluteEnvTN))
>               signal = OscI AR ampenv (PchToHz notePit) 
>                                       (Const (float fluteWaveTN)) 
>           in  StereoOut signal signal
>
> fluteOrc = let i = (inst1, fluteOE)
>            in  (hdr, [i])
>
> flute = playCS fluteOrc fluteScore




