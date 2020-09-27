This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

< openFile    :: FilePath -> IOMode -> IO Handle
< hClose      :: Handle -> IO () 
< data IOMode  = ReadMode   | WriteMode 
<              | AppendMode | ReadWriteMode

< hPutChar  :: Handle -> Char -> IO ()
< hPutStr   :: Handle -> String -> IO ()
< hPutStrLn :: Handle -> String -> IO ()
< hPrint    :: Show a => Handle -> a -> IO ()

< hGetChar  :: Handle -> IO Char
< hGetLine  :: Handle -> IO String

< hGetContents :: Handle -> String

< writeFile :: FilePath -> String -> IO ()
< type FilePath = String

< appendFile :: FilePath -> String -> IO ()

< getChar = hGetChar stdin
< putChar = hPutChar stdout

< isEOFError :: IOError -> Bool

< catch :: IO a -> (IOError -> IO a) -> IO a

< getChar' :: IO Char
< getChar'  = catch getChar (\e -> return '\n')

< getChar' :: IO Char
< getChar'  = catch getChar (\e -> if isEOFError e then return '\n'
<                                                  else ioError e)

< getLine':: IO String
< getLine' = catch getLine'' (\err -> "Error: " ++ show err)
<            where getLine'' = do c <- getChar'
<                                 if c == '\n' then return ""
<                                              else do l <- getLine'
<                                                      return (c:l)

> module Main where
> import IO
>
> getAndOpenFile :: String -> IOMode -> IO Handle

> getAndOpenFile prompt mode
>   = do putStr prompt
>        name <- getLine
>        catch (do handle <- openFile name mode 
>                  return handle)
>              (\error -> do putStrLn ("Cannot open " ++ name)
>                            print error
>                            getAndOpenFile prompt mode)
>          

> main = do fromHandle <- getAndOpenFile "Copy from: " ReadMode
>           toHandle   <- getAndOpenFile "Copy to: "   WriteMode 
>           contents   <- hGetContents fromHandle
>           hPutStr toHandle contents
>           hClose fromHandle
>           hClose toHandle
>           putStrLn "Done."

< newChan         :: IO (Chan a)
< writeChan       :: Chan a -> a -> IO ()
< readChan        :: Chan a -> IO a
< getChanContents :: Chan a -> IO [a]
< isEmptyChan     :: Chan a -> IO Bool

< do c <- newChan
<    writeChan c `a`
<    writeChan c `b`
<    ...

< do ...
<    a <- readChan c
<    b <- readChan c
<    return [a,b]

< forkIO :: IO () -> IO ()

> module ChannelTest where
>
> import Channel
> import ConcBase
> 
> main :: IO ()
> main = do c1 <- newChan :: IO (Chan Int)
>           c2 <- newChan :: IO (Chan Int)
>           forkIO (client c1 c2)
>           forkIO (server c2 c1)
>
> client :: Chan Int -> Chan Int -> IO ()
> client cin cout
>   = do writeChan cout 1
>        loop
>     where loop = do c <- readChan cin
>                     print c
>                     writeChan cout c
>                     loop
>
> server :: Chan Int -> Chan Int -> IO ()
> server cin cout
>   = do loop
>     where loop = do c <- readChan cin
>                     writeChan cout (c+1)
>                     loop

