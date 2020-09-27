This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

< module Reactimate where
< import Fal
< import SOE hiding (Region, Event)
< import qualified SOE as G (Region, Event)
< import Draw (xWin,yWin,intToFloat)
< import Win32Misc (timeGetTime)
< -- import Word (word32ToInt)
< import Channel

> module Reactimate where
> import Fal hiding (reactimate, sample, windowUser, makeStream, w32ToTime)
> import SOE hiding (Region, Event)
> import qualified SOE as G (Region, Event)
> import Draw (xWin,yWin,intToFloat)
> -- import Word (word32ToInt)
> import Control.Concurrent

< reactimate :: String -> Behavior a -> (a -> IO Graphic) -> IO ()

< windowUser :: Window -> IO (([Maybe UserAction],[Time]), IO ())

| ((us,ts),addEvents) <- windowUser w

> sample :: Event ()
> sample = Event (\(us,_) -> map aux us)
>   where aux Nothing  = Just ()
>         aux (Just _) = Nothing

| sample `snapshot_` franProg 

> reactimate 
>   :: String -> Behavior a -> (a -> IO Graphic) -> IO ()
> reactimate title franProg toGraphic
>   = runGraphics $
>     do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
>               drawBufferedGraphic
>        (user, addEvents) <- windowUser w
>        addEvents
>        let drawPic (Just p) = 
>              do g <- toGraphic p 
>                 setGraphic w g
>                 addEvents
>            drawPic Nothing  = return ()
>        mapM_ drawPic 
>              (runEvent (sample `snapshot_` franProg) user)

> runEvent (Event fe) u = fe u

> makeStream :: IO ([a], a -> IO ())

> makeStream 
>   = do ch <- newChan
>        contents <- getChanContents ch
>        return (contents, writeChan ch)

> windowUser 
>   :: Window -> IO (([Maybe UserAction],[Time]), IO ())
> windowUser w
>   = do (evs, addEv) <- makeStream
>        t0 <- timeGetTime
>        let loop rt =
>              do mev <- maybeGetWindowEvent w
>                 case mev of
>                   Nothing -> return ()
>                   Just e  -> do addEv (Just e, rt)
>                                 loop rt
>        let addEvents =
>              do t <- timeGetTime
>                 let rt = w32ToTime (t-t0)
>                 loop rt
>                 addEv (Nothing, rt)
>        return ((map fst evs, map snd evs), addEvents)

> w32ToTime t = intToFloat (fromInteger (toInteger t)) / 1000

