\section{Reading Midi files}
\label{readMidi}

Now that we have translated a raw Midi file into a {\tt MidiFile} data
type, we can translate that {\tt MidiFile} into a {\tt Music} object.

\begin{verbatim}

> module ReadMidi (readMidi, module Performance, module MidiFile) 
>   where
>
> import Data.Ratio
> import Basics
> import Performance
> import MidiFile
> import HaskToMidi
> import GeneralMidi

\end{verbatim}

The main function.  Note that we output a Context and a UserPatchMap
as well as a Music object.
\begin{verbatim}

> readMidi :: MidiFile -> (Music, Context, UserPatchMap)
> readMidi mf@(MidiFile _ d trks) =
>   let trk1        = removeEOTs $ format mf
>       upm         = makeUPM trks
>       (trk2,dur)  = getFirst trk1
>       trksrs      = getRest dur (newTracks dur (splitBy (isTempoChg) trk2))
>       read (t,r)  = Tempo r (readTrack (tDiv d) upm t)
>       m           = optimize $ line $ map read trksrs
>   in (m,
>       Context {cTime   = 0,
>                cPlayer = fancyPlayer,
>                cInst   = getInst upm,
>                cDur    = float dur / 250000,
>                cKey    = 0,
>                cVol    = 100},
>       upm)

\end{verbatim}

Remove the {\tt EndOfTrack}'s that end up in the middle of merged or
sequenced tracks.
\begin{verbatim}

> removeEOTs :: Track -> Track
> removeEOTs ((MetaEvent _ EndOfTrack):es) = removeEOTs es
> removeEOTs (e:es) = e:(removeEOTs es)
> removeEOTs [] = []

\end{verbatim}

Make one big track out of the individual tracks of a MidiFile, using
different methods depending on the format of the MidiFile.
\begin{verbatim}

> format (MidiFile 0 _ [trk]) = trk
> format (MidiFile 1 _ trks)  = mergeTracks trks
> format (MidiFile 2 _ trks)  = seqTracks trks
> format _                    = error ("readMidi: unfamiliar format or file corrupt")

\end{verbatim}

Used for a list of simultaneous tracks (format 1).  It merges together
the events of all the tracks, putting the events in order by their
elapsed time.  A generalization of this algorithm could be useful as a
merge of an arbitrary number of lists.
\begin{verbatim}

> mergeTracks :: [Track] -> Track
> mergeTracks [[]]      = []
> mergeTracks ([]:trks) = mergeTracks trks
> mergeTracks trks      = let minTs = foldl1 minT trks
>                             minT []              trk             = trk
>                             minT trk             []              = trk
>                             minT (trk1@(e1:e1s)) (trk2@(e2:e2s)) =
>                               if getTime e1 <= getTime e2 then trk1 else trk2
>                             newList (trk@(e:es):trks) trk'
>                               | e == (head trk') = es:trks
>                               | otherwise        = trk:(newList trks trk')
>                             newList ([]:trks) trk' = []:newList trks trk'
>                             newList ([])      _    = [[]]
>                         in
>                          (head (minTs)) : mergeTracks (newList trks (minTs))

\end{verbatim}

Used for a list of sequential tracks (format 2).  It puts the events
of the tracks in order one after the other.  About its auxiliary
functions: {\tt seqTracks'} is the same as seqTracks, only it keeps a
running total of the time increment; {\tt incTrack} increments the
elapsed-time of each event in the given track; {\tt incEvent}
increments the elapsed-time of a single event.
\begin{verbatim}

> seqTracks :: [Track] -> Track
> seqTracks trks      = seqTracks' 0 trks
>   where
>     seqTracks' _ []            = []
>     seqTracks' _ [[]]          = []
>     seqTracks' n ([]:trks)     = seqTracks' n trks
>     seqTracks' n (trk:trks)    = a ++ seqTracks' (b+n) trks
>        where (a,b) = incTrack n trk
>     incTrack n []              = error ("incTrack: Empty list")
>     incTrack n [e]             = ([incEvent n e],getTime e)
>     incTrack n (e:es) = ((incEvent n e):a, b)
>       where (a,b) = incTrack n es
>     incEvent n (MidiEvent t x) = MidiEvent (t+n) x
>     incEvent n (MetaEvent t x) = MetaEvent (t+n) x

\end{verbatim}

Get the elapsed time of an event.
\begin{verbatim}

> getTime :: MEvent -> ElapsedTime
> getTime (MidiEvent t _) = t
> getTime (MetaEvent t _) = t

\end{verbatim}

Look through the given tracks, using Program Changes to make a
UserPatchMap.
\begin{verbatim}

> makeUPM :: [Track] -> UserPatchMap
> makeUPM trks = removeDups $ map makeTriple $ concatMap searchPC trks
>   where makeTriple (ch,num) = let gmName = lookupInst genMidiMap num
>                                   iName = case ch of
>                                             9 -> "drums"
>                                             _ -> gmName
>                               in (iName, gmName, ch)
>         searchPC ((MidiEvent _ (ProgChange ch num)):es)    = (ch, num) : searchPC es
>         searchPC ((MidiEvent t (NoteOn _ _ _)):es) | t > 0 = []
>         searchPC (e:es)                                    = searchPC es
>         searchPC []                                        = []
>         searchInst ((MetaEvent _ (InstrName iName)):es)      = Just iName
>         searchInst ((MidiEvent t (NoteOn _ _ _)):es) | t > 0 = Nothing
>         searchInst (e:es)                                    = searchInst es
>         searchInst []                                        = Nothing

\end{verbatim}

Remove consecutive duplicates from a list, used in this case to take
out redundant elements in the UserPatchMap.
\begin{verbatim}

> removeDups :: Eq a => [a] -> [a]
> removeDups []        = []
> removeDups [x]       = [x]
> removeDups (x:y:xys) | x == y    = removeDups (y:xys)
>                      | otherwise = x : removeDups (y:xys)

\end{verbatim}

Translate Divisions into the number of ticks per quarter note.
\begin{verbatim}

> tDiv :: Division -> Int
> tDiv (Ticks x) = x
> tDiv (SMPTE _ _) = error "Sorry, SMPTE not yet implemented."

\end{verbatim}

{\tt getFirst} gets the information that occurs at the beginning of
the piece: the default tempo and the default key signature.  A
SetTempo in the middle of the piece should translate to a tempo change
({\tt Tempo x y m}), but a SetTempo at time 0 should set the default
tempo for the entire piece, by translating to Context tempo.  {\tt
getFirst} takes care of all events that occur at time 0 so that if any
SetTempo appears at time 0, it can translate to Context even if it is
not the very first event.
\begin{verbatim}

> getFirst :: Track -> (Track, Int)
> getFirst trk1 = getFirst' trk1 []
>  where
>   getFirst' :: Track -> Track -> (Track, Int)
>   getFirst' ((MetaEvent 0 (SetTempo tempo)):es) tAcc = (((reverse tAcc)++es), tempo)
>   getFirst' (e@(MetaEvent 0 _):es)              tAcc = getFirst' es (e:tAcc)
>   getFirst' (e@(MidiEvent 0 _):es)              tAcc = getFirst' es (e:tAcc)
>   getFirst' []                                  _    = ([]                  , defST)
>   getFirst' es                                  tAcc = (((reverse tAcc)++es), defST)

\end{verbatim}

Manages the tempo changes in the piece; it translates each MidiFile
{\tt SetTempo} into a ratio between the new tempo and the tempo at the
beginning.
\begin{verbatim}

> getRest :: Int -> [Track] -> [(Track,Ratio Int)]
> getRest d (((MetaEvent _ (SetTempo tempo)):es):trks) = (es,r) : getRest d trks
>   where r = d % tempo
> getRest d (trk:trks) = (trk,1) : getRest d trks
> getRest _ []         = []

\end{verbatim}

Get the first instrument from the UserPatchMap, to use as the default
in the Context.
\begin{verbatim}

> getInst :: UserPatchMap -> String
> getInst ((iName, gmName, channel):xs) = iName
> getInst []                            = "piano"

\end{verbatim}

{\tt splitBy} takes a boolean test and a list; it divides up the list
and turns it into a {\em list of sub-lists}; each sub-list consists of
(1) one element for which the test is true (or the first element in
the list), and (2) all elements after that element for which the test
is false.  Used to split a track into sub-tracks by tempo.  For
example, {\tt splitBy (>10) [27, 0, 2, 1, 15, 3, 42, 4]} yields
{\tt [ [27,0,2,1], [15,3], [42,4] ]}.
\begin{verbatim}

> splitBy :: (a -> Bool) -> [a] -> [[a]]
> splitBy test xs = splitBy' test xs []
>   where splitBy' :: (a -> Bool) -> [a] -> [a] -> [[a]]
>         splitBy' test (x1:x2:xs) acc = 
>           if (test x2)
>           then (reverse (x1:acc)) : (splitBy' test (x2:xs) [])
>           else splitBy' test (x2:xs) (x1:acc)
>         splitBy' test [x] acc = [(reverse (x:acc))]
>         splitBy' _    []  _   = [[]]

> isTempoChg :: MEvent -> Bool
> isTempoChg (MetaEvent _ (SetTempo _)) = True
> isTempoChg _                          = False

\end{verbatim}

{\tt readTrack} is the heart of the {\tt readMidi} operation.  It
reads a track that has been "processed" by {\tt newTracks}, and
returns the track as Music.
\begin{verbatim}

> readTrack :: Int -> UserPatchMap -> Track -> Music
> readTrack _     _   []
>     = Rest 0
> readTrack ticks upm (nOn @(MidiEvent t1 (NoteOn  ch p v))
>                    : nOff@(MidiEvent t2 (NoteOff _  _ _))
>                    : nOn2@(MidiEvent t3 (NoteOn  _  _ _))
>                    : es)
>   | (t2 < t1) || (t3 < t1) = error "readTrack: elapsed time values out of order"
>   | t2 == t3        = n :+: readTrack ticks upm (nOn2:es)
>   | t1 == t3        = n :=: readTrack ticks upm (nOn2:es)
>   | t3 < t2         = n :=: (Rest (diff ticks t1 t3) :+: readTrack ticks upm (nOn2:es))
>   | t3 > t2         = n :+:  Rest (diff ticks t2 t3) :+: readTrack ticks upm (nOn2:es)
>        where plainNote = Note (pitch p) (diff ticks t1 t2) (makeVol v)
>              n = if ch == 9 then Instr "drums"            plainNote
>                             else Instr (lookupUPM ch upm) plainNote
> readTrack ticks upm (nOn@ (MidiEvent _ (NoteOn  _ _ _))
>                    : nOff@(MidiEvent _ (NoteOff _ _ _))
>                    : x
>                    : es)
>   = readTrack ticks upm (x : nOn : nOff : es)
> readTrack ticks upm (MidiEvent t1 (NoteOn  ch p v)
>                    : MidiEvent t2 (NoteOff _  _ _)
>                    : es)
>   | t2 < t1   = error "readTrack: elapsed time values of last note out of order"
>   | otherwise = n :+: readTrack ticks upm es
>      where plainNote = Note (pitch p) (diff ticks t1 t2) (makeVol v)
>            n = if ch == 9 then Instr "drums"            plainNote
>                           else Instr (lookupUPM ch upm) plainNote
> readTrack ticks upm ((MidiEvent t (ProgChange ch num)):es)
>     = readTrack ticks (progChange ch num upm) es
> readTrack ticks upm (e:es)
>     = readTrack ticks upm es

\end{verbatim}

Take the division in ticks and two time values and calculates the note
duration (quarter note, eighth note, etc.) that expresses their difference.
\begin{verbatim}

> diff :: Int -> Int -> Int -> Dur
> diff ticks t1 t2 = ((t2 - t1) % ticks) / 8

\end{verbatim}

Look up an instrument name from a UserPatchMap given its channel number.
\begin{verbatim}

> lookupUPM :: Int -> UserPatchMap -> String
> lookupUPM ch ((name,_,midiChan):xs)
>   | ch == midiChan = name
>   | otherwise      = lookupUPM ch xs
> lookupUPM ch  [] = error ("Invalid channel in user patch map")

\end{verbatim}

Implement a {\em Program Change}: a change in the UserPatchMap in
which a channel changes from one instrument to another.
\begin{verbatim}

> progChange :: Int -> Int -> UserPatchMap -> UserPatchMap
> progChange ch num ((_, _, midiChan):xs) | ch == midiChan =
>   let n = lookupInst genMidiMap num
>   in ((n,n,ch):xs)
> progChange ch num (x:xs) = x:(progChange ch num xs)
> progChange _  _   []     = []

\end{verbatim}

Look up an instrument in the General Midi table given its program number.
\begin{verbatim}

> lookupInst :: GenMidiTable -> Int -> IName
> lookupInst ((inst,num):xs) n | (num == n) = inst
>                              | otherwise  = lookupInst xs n

\end{verbatim}

Interpret a MidiFile velocity, creating a Music NoteAttribute.  The
MIDI specification calls for some sort of exponential scale, but for
now it's just linear.
\begin{verbatim}

> makeVol :: Int -> [NoteAttribute]
> makeVol x = [Volume (float x)]

\end{verbatim}

The {\tt newTracks} function changes the order of the events in a list
of sub-tracks so that they can be handled by readTrack: each NoteOff
is put directly after its corresponding NoteOn. Its first and second
arguments are the elapsed time and value (in microseconds per quarter
note) of the SetTempo currently in effect.
\begin{verbatim}

> newTracks :: Int -> [Track] -> [Track]
> newTracks dur = newTracks' 0 dur
>   where
>     newTracks' :: Int -> Int -> [Track] -> [Track]
>     newTracks' _   _   []        = []
>     newTracks' _   _   [[]]      = [[]]
>     newTracks' stt stv ([]:trks) = [] : newTracks' stt stv trks
>     newTracks' _   _   ((e@(MetaEvent newStt (SetTempo newStv)):es):trks) =
>       let (trk':trks') = newTracks' newStt newStv (es:trks)
>       in ((e:trk'):trks')
>     newTracks' stt stv ((e1@(MidiEvent tno (NoteOn c p _)):es):trks) =
>       let (e2, leftover) = search (stt-tno) stv stv tno stt c p (es:trks) [[]]
>           (trk':trks') = newTracks' stt stv leftover
>       in ((e1:e2:trk'):trks')
>     newTracks' stt stv ((e:es):trks) = let (trk':trks') = newTracks' stt stv (es:trks)
>                                        in ((e:trk'):trks')

\end{verbatim}

{\tt search} takes a track that has been divided by tempo into
sub-lists; it looks through the list of sub-tracks to find the NoteOff
corresponding to the given NoteOn.  A NoteOff corresponds to an
earlier NoteOn if it is the first in the track to have the same
channel and pitch.  If it's in a different sub-track than its NoteOn,
it puts it in the NoteOn's subtrack and calculates what the new
elapsed-time should be after one or more tempo changes.  This function
takes a ridiculous number of arguments, I know, but I don't think it
can do without any of the information.  Maybe there is a simpler way.
\begin{verbatim}

> search :: Int ->           -- time interval between NoteOn and most recent SetTempo,
>                            --   in terms of the tempo at the NoteOn
>           Int -> Int ->    -- SetTempo values: the one at the NoteOn and the most
>                            --   recent one
>           Int -> Int ->    -- elapsed times of the NoteOn and the most recent SetTempo
>           Int -> Int ->    -- channel and pitch of NoteOn (NoteOff must match)
>           [Track] ->       -- the tracks left to be searched
>           [Track] ->       -- accumulator: what's left after NoteOff is found
>           (MEvent,[Track]) -- the needed event and the remainder of the tracks
> search int ost nst tno stt c1 p1 ((e@(MidiEvent t (NoteOff c2 p2 v)):es):trks)
>                                                                      (aTrk:aTrks)
>   | c1 == c2 && p1 == p2
>   = ((MidiEvent eTime (NoteOff c2 p2 v)),
>      (reverse aTrks)++(((reverse aTrk)++es):trks))
>     where eTime = tno + int + round ((float (t-stt))*(float ost)/(float nst))
> search int ost nst tno stt c  p  ((e@(MetaEvent t (SetTempo st)):es):trks)
>                                                                      (aTrk:aTrks) =
>      search newInt ost st  tno t   c p (es:trks) ((e:aTrk):aTrks)
>        where newInt = int + round ((float (t-stt))*(float ost)/(float nst))
> search _   _   _   tno _   c  p  [[]] (aTrk:aTrks) =
>      ((MidiEvent tno (NoteOff c p 0)), aTrks++[reverse aTrk])
> search _   _   _   tno _   c  p  []   (aTrk:aTrks) =
>      ((MidiEvent tno (NoteOff c p 0)), aTrks++[reverse aTrk])
> search int ost nst tno stt c  p  ((e:es):trks) (aTrk:aTrks) =
>      search int    ost nst tno stt c p (es:trks) ((e:aTrk):aTrks)
> search int ost nst tno stt c  p  ([]    :trks) (aTrk:aTrks) =
>      search int    ost nst tno stt c p trks      ([]:(reverse aTrk):aTrks)
> search _   _   _   _   _   _  _  []            _            =
>                                  error "newTracks: search fell thru"

\end{verbatim}

Music objects that come out of {\tt readMidi} almost always contain
redundancies, like rests of zero duration and redundant instrument
specifications.  {\tt optimize} reduces the redundancy to make a Music
file less cluttered and more efficient to use.
\begin{verbatim}

> optimize, optRests, optTempo, optInstr, optVol :: Music -> Music
> optimize m = optTempo $ optVol $ optInstr $ optRests m

\end{verbatim}

Remove rests of zero duration.
\begin{verbatim}

> optRests (m :+: Rest 0) = optRests m
> optRests (m :=: Rest 0) = optRests m
> optRests (Rest 0 :+: m) = optRests m
> optRests (Rest 0 :=: m) = optRests m
> optRests (m1 :+: m2) = optRests m1 :+: optRests m2
> optRests (m1 :=: m2) = optRests m1 :=: optRests m2
> optRests (Tempo a m)   = Tempo a  $ optRests m
> optRests (Trans a m)   = Trans a  $ optRests m
> optRests (Instr a m)   = Instr a  $ optRests m
> optRests (Player a m)  = Player a $ optRests m
> optRests (Phrase a m)  = Phrase a $ optRests m
> optRests x             = x

\end{verbatim}

Remove redundant {\tt Tempo}'s.
\begin{verbatim}

> optTempo (Tempo 1 m) = optTempo m
> optTempo (Tempo a (Tempo b m)) = Tempo (a*b) $ optTempo m
> optTempo (m1 :+: m2) = optTempo m1 :+: optTempo m2
> optTempo (m1 :=: m2) = optTempo m1 :=: optTempo m2
> optTempo (Tempo a m)   = Tempo a   $ optTempo m
> optTempo (Trans a m)   = Trans a   $ optTempo m
> optTempo (Instr a m)   = Instr a   $ optTempo m
> optTempo (Player a m)  = Player a  $ optTempo m
> optTempo (Phrase a m)  = Phrase a  $ optTempo m
> optTempo x             = x

\end{verbatim}

Remove unnecessary {\tt Instr}'s.
\begin{verbatim}

> optInstr m1 = let (m2,changed) = optInstr' m1
>              in if changed then optInstr m2
>                            else m2
>   where
>     optInstr' :: Music -> (Music, Bool)
>     optInstr' ((m1 :+: m2) :+: m3) = (m1 :+: m2 :+: m3, True)
>     optInstr' ((m1 :=: m2) :=: m3) = (m1 :=: m2 :=: m3, True)
>     optInstr' (Instr a (Instr b m)) = (Instr a m, True)
>     optInstr' (Instr a m1 :+: Instr b m2) | a == b = (Instr a (m1 :+: m2), True)
>     optInstr' (Instr a m1 :=: Instr b m2) | a == b = (Instr a (m1 :=: m2), True)
>     optInstr' (Instr a m1 :+: Instr b m2 :+: x)
>       | a == b = (Instr a (m1 :+: m2) :+: x, True)
>     optInstr' (Instr a m1 :=: Instr b m2 :=: x)
>       | a == b = (Instr a (m1 :=: m2) :=: x, True)
>     optInstr' (Instr a m :+: Rest r) = (Instr a (m :+: Rest r), True)
>     optInstr' (Instr a m :=: Rest r) = (Instr a (m :=: Rest r), True)
>     optInstr' (Rest r :+: Instr a m) = (Instr a (Rest r :+: m), True)
>     optInstr' (Rest r :=: Instr a m) = (Instr a (Rest r :=: m), True)
>     optInstr' (m1 :+: m2) = let (m3,c3) = optInstr' m1
>                                 (m4,c4) = optInstr' m2
>                             in (m3 :+: m4, c3 || c4)
>     optInstr' (m1 :=: m2) = let (m3,c3) = optInstr' m1
>                                 (m4,c4) = optInstr' m2
>                             in (m3 :=: m4, c3 || c4)
>     optInstr' (Tempo a m)   = (Tempo a   $ optInstr m, False)
>     optInstr' (Trans a m)   = (Trans a   $ optInstr m, False)
>     optInstr' (Instr a m)   = (Instr a   $ optInstr m, False)
>     optInstr' (Player a m)  = (Player a  $ optInstr m, False)
>     optInstr' (Phrase a m)  = (Phrase a  $ optInstr m, False)
>     optInstr' x             = (x, False)

\end{verbatim}

Change repeated Volume Note Attributes to Phrase Attributes.
\begin{verbatim}

> optVol m1 = let (m2,changed) = optVol' m1
>             in if changed then optVol m2
>                           else m2
> optVol' ((m1 :+: m2) :+: m3) = (m1 :+: m2 :+: m3, True)
> optVol' ((m1 :=: m2) :=: m3) = (m1 :=: m2 :=: m3, True)
> optVol' (Phrase p1 (Phrase p2 m)) | p1 == p2  = (Phrase p1 m, True)
>                                   | otherwise = (Phrase (p1 ++ p2) m, True)
> optVol' (Note p1 d1 [Volume v1] :+: Note p2 d2 [Volume v2]) | v1 == v2 =
>     (Phrase [Dyn (Loudness v1)] (Note p1 d1 [] :+: Note p2 d2 []), True)
> optVol' (Note p1 d1 [Volume v1] :=: Note p2 d2 [Volume v2]) | v1 == v2 =
>     (Phrase [Dyn (Loudness v1)] (Note p1 d1 [] :=: Note p2 d2 []), True)
> optVol' (Note p1 d1 [Volume v1] :+: Note p2 d2 [Volume v2] :+: x) | v1 == v2 =
>    ((Phrase [Dyn (Loudness v1)] (Note p1 d1 [] :+: Note p2 d2 [])) :+: x, True)
> optVol' (Note p1 d1 [Volume v1] :=: Note p2 d2 [Volume v2] :=: x) | v1 == v2 =
>    ((Phrase [Dyn (Loudness v1)] (Note p1 d1 [] :=: Note p2 d2 [])) :=: x, True)
> optVol' (Phrase p1 m1 :+: Phrase p2 m2) | p1 == p2 = (Phrase p1 (m1 :+: m2), True)
> optVol' (Phrase p1 m1 :=: Phrase p2 m2) | p1 == p2 = (Phrase p1 (m1 :=: m2), True)
> optVol' (Phrase p1 m1 :+: Phrase p2 m2 :+: x) | p1 == p2 =
>     (Phrase p1 (m1 :+: m2 :+: x),True)
> optVol' (Phrase p1 m1 :=: Phrase p2 m2 :=: x) | p1 == p2 =
>     (Phrase p1 (m1 :=: m2 :=: x),True)
> optVol' (Phrase phr@[Dyn (Loudness l)] m :+: Note p d [Volume v]) | l == v =
>     (Phrase phr (m :+: Note p d []), True)
> optVol' (Phrase phr@[Dyn (Loudness l)] m :=: Note p d [Volume v]) | l == v =
>     (Phrase phr (m :=: Note p d []), True)
> optVol' (Phrase phr@[Dyn (Loudness l)] m :+: Note p d [Volume v] :+: x) | l == v =
>     (Phrase phr (m :+: Note p d []) :+: x,True)
> optVol' (Phrase phr@[Dyn (Loudness l)] m :=: Note p d [Volume v] :=: x) | l == v =
>     (Phrase phr (m :=: Note p d []) :=: x,True)
> optVol' (Note p d [Volume v] :+: Phrase phr@[Dyn (Loudness l)] m) | l == v =
>     (Phrase phr (Note p d [] :+: m),True)
> optVol' (Note p d [Volume v] :=: Phrase phr@[Dyn (Loudness l)] m) | l == v =
>     (Phrase phr (Note p d [] :=: m),True)
> optVol' (Note p d [Volume v] :+: Phrase phr@[Dyn (Loudness l)] m :+: x) | l == v =
>     ((Phrase phr (Note p d [] :+: m)) :+: x, True)
> optVol' (Note p d [Volume v] :=: Phrase phr@[Dyn (Loudness l)] m :=: x) | l == v =
>     ((Phrase phr (Note p d [] :=: m)) :=: x, True)
> optVol' (Rest r :+: Note p d [Volume v]) =
>     (Phrase [Dyn (Loudness v)] (Rest r :+: Note p d []),True)
> optVol' (Rest r :=: Note p d [Volume v]) =
>     (Phrase [Dyn (Loudness v)] (Rest r :=: Note p d []),True)
> optVol' (Note p d [Volume v] :+: Rest r) =
>     (Phrase [Dyn (Loudness v)] (Note p d [] :+: Rest r),True)
> optVol' (Note p d [Volume v] :=: Rest r) =
>     (Phrase [Dyn (Loudness v)] (Note p d [] :=: Rest r),True)
> optVol' (Rest r :+: Phrase p m) = (Phrase p (Rest r :+: m), True)
> optVol' (Rest r :=: Phrase p m) = (Phrase p (Rest r :=: m), True)
> optVol' (Phrase p m :+: Rest r) = (Phrase p (m :+: Rest r), True)
> optVol' (Phrase p m :=: Rest r) = (Phrase p (m :=: Rest r), True)
> optVol' (m1 :+: m2) = let (m3,c3) = optVol' m1
>                           (m4,c4) = optVol' m2
>                       in (m3 :+: m4, c3 || c4)
> optVol' (m1 :=: m2) = let (m3,c3) = optVol' m1
>                           (m4,c4) = optVol' m2
>                       in (m3 :=: m4, c3 || c4)
> optVol' (Tempo a m)   = (Tempo a   $ optVol m, False)
> optVol' (Trans a m)   = (Trans a   $ optVol m, False)
> optVol' (Instr a m)   = (Instr a   $ optVol m, False)
> optVol' (Player a m)  = (Player a  $ optVol m, False)
> optVol' (Phrase a m)  = (Phrase a  $ optVol m, False)
> optVol' x             = (x, False)

\end{verbatim}
