\begin{verbatim}

> module MidiFile where
>
> import System.IO

\end{verbatim} 

{\tt OutputMidiFile} is the main function for writing {\tt MidiFile}
values to an actual file; its first argument is the filename:
\begin{verbatim} 

> outputMidiFile :: String -> MidiFile -> IO ()
> outputMidiFile fn mf = do
>       h <- openBinaryFile fn WriteMode
>       hPutStr h (midiFileToString mf)
>       hClose h

\end{verbatim} 

\begin{exercise} Take as many examples as you like from the previous
sections, create one or more {\tt UserPatchMaps}, write the examples
to a file, and play them using a conventional Midi player.
\end{exercise}
Appendix \ref{test-functions} defines some functions which should make
the above exercise easier.  Appendices \ref{examples}, \ref{chick},
and \ref{self-similar} contain more extensive examples.
