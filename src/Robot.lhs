This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module Robot where
>
> import Data.Array
> import Data.List
> import Control.Monad
> import SOE

> -- import qualified GraphicsWindows as GW (getEvent)

< drawSquare
< = do penDown
<      move
<      turnRight
<      move
<      turnRight
<      move
<      turnRight
<      move

< cond :: Robot Bool -> Robot a -> Robot a -> Robot a

< evade :: Robot ()
< evade = cond blocked
<           (do turnRight
<               move)
<           move

> evade :: Robot ()
> evade = do cond1 blocked turnRight 
>            move

> moveToWall :: Robot ()
> moveToWall = while (isnt blocked)
>                move

> getCoinsToWall :: Robot ()
> getCoinsToWall = while (isnt blocked) $
>                    do move
>                       pickCoin

> spiral :: Robot ()
> spiral = penDown >> loop 1
>  where loop n =
>          let twice = do turnRight
>                         moven n
>                         turnRight
>                         moven n
>          in cond blocked 
>               (twice >> turnRight >> moven n)
>               (twice >> loop (n+1))

> moven :: Int -> Robot ()
> moven n = mapM_ (const move) [1..n]

> data RobotState 
>   = RobotState 
>         { position  :: Position
>         , facing    :: Direction
>         , pen       :: Bool 
>         , color     :: Color
>         , treasure  :: [Position]
>         , pocket    :: Int
>         }
>      deriving Show

> type Position = (Int,Int)

> data Direction = North | East | South | West
>      deriving (Eq,Show,Enum)

< type Robot1 a = RobotState -> Grid -> (RobotState, a)

< type Robot2 a = RobotState -> Grid -> Window -> (RobotState, a, IO ())

< type Robot3 a = RobotState -> Grid -> Window -> IO (RobotState, a)

> newtype Robot a 
>   = Robot (RobotState -> Grid -> Window -> IO (RobotState, a))

> instance Functor Robot where
>   fmap = liftM
> instance Applicative Robot where
>   pure = return
>   (<*>) = ap

> instance Monad Robot where
>   return a 
>     = Robot (\s _ _ -> return (s,a))
>   Robot sf0 >>= f
>     = Robot $ \s0 g w -> do
>                 (s1,a1) <- sf0 s0 g w
>                 let Robot sf1 = f a1
>                 (s2,a2) <- sf1 s1 g w
>                 return (s2,a2)

> right,left :: Direction -> Direction

> right d = toEnum (succ (fromEnum d) `mod` 4)
> left  d = toEnum (pred (fromEnum d) `mod` 4)

> updateState :: (RobotState -> RobotState) -> Robot ()
> updateState u = Robot (\s _ _ -> return (u s, ()))

> queryState  :: (RobotState -> a) -> Robot a
> queryState  q = Robot (\s _ _ -> return (s, q s))

> turnLeft  :: Robot ()
> turnLeft = updateState (\s -> s {facing = left (facing s)})

> turnRight :: Robot ()
> turnRight = updateState (\s -> s {facing = right (facing s)})

> turnTo    :: Direction -> Robot ()
> turnTo d = updateState (\s -> s {facing = d})

> direction :: Robot Direction
> direction = queryState facing

> blocked   :: Robot Bool
> blocked = Robot $ \s g _ -> 
>             return (s, facing s `notElem` (g `at` position s))

> move      :: Robot ()
> move = cond1 (isnt blocked)
>          (Robot $ \s _ w -> do
>             let newPos = movePos (position s) (facing s)
>             graphicsMove w s newPos
>             return (s {position = newPos}, ())
>          )

> movePos :: Position -> Direction -> Position
> movePos (x,y) d
>   = case d of
>       North -> (x,y+1)
>       South -> (x,y-1)
>       East  -> (x+1,y)
>       West  -> (x-1,y)

> penUp :: Robot ()
> penUp = updateState (\s -> s {pen = False})

> penDown :: Robot ()
> penDown = updateState (\s -> s {pen = True})

> setPenColor  :: Color -> Robot ()
> setPenColor c = updateState (\s -> s {color = c})

> onCoin    :: Robot Bool
> onCoin = queryState (\s -> position s `elem` treasure s)

> coins     :: Robot Int
> coins  = queryState pocket

> pickCoin  :: Robot ()
> pickCoin = cond1 onCoin
>              (Robot $ \s _ w -> 
>                 do eraseCoin w (position s)
>                    return (s {treasure = position s `delete` treasure s, 
>                               pocket   = pocket s + 1}, ()              )
>              )

> dropCoin  :: Robot ()
> dropCoin = cond1 (coins >* return 0)
>              (Robot $ \s _ w -> 
>                 do drawCoin w (position s)
>                    return (s {treasure = position s : treasure s,
>                               pocket   = pocket s - 1}, ()       )
>              )

< liftM    :: (Monad m) => (a -> b) -> (m a -> m b)
< liftM f  =  \a -> do a' <- a
<                      return (f a')

< liftM2   :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
< liftM2 f =  \a b -> do a' <- a
<                        b' <- b
<                        return (f a' b')

> cond      :: Robot Bool -> Robot a -> Robot a -> Robot a
> cond p c a = do pred <- p
>                 if pred then c else a

> cond1 p c = cond p c (return ())

> while     :: Robot Bool -> Robot () -> Robot ()
> while p b = cond1 p (b >> while p b)

> (||*)     :: Robot Bool -> Robot Bool -> Robot Bool
> b1 ||* b2 = do p <- b1
>                if p then return True
>                     else b2

> (&&*)     :: Robot Bool -> Robot Bool -> Robot Bool
> b1 &&* b2 = do p <- b1
>                if p then b2
>                     else return False

> isnt :: Robot Bool -> Robot Bool
> isnt = liftM not

> (>*),(<*) :: Robot Int -> Robot Int -> Robot Bool
> (>*) = liftM2 (>)
> (<*) = liftM2 (<)

< array :: Ix a => (a,a) -> [(a,b)] -> Array a b

< colors :: Array Int Color
< colors = array (0,7) [(0,Black), (1,Blue), (2,Green), (3,Cyan)
<                    (4,Red), (5,Magenta), (6,Yellow), (7,White)]

< data Color = Black | Blue | Green | Cyan
<            | Red | Magenta | Yellow | White
<      deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

> colors :: Array Int Color
> colors = array (0,7) (zip [0..7] [Black .. White])

> type Grid = Array Position [Direction]

< at :: Grid -> Position -> [Direction]
< at = (!)

> at :: Grid -> Position -> [Direction]
> at = (!)

> size :: Int
> size = 20

> interior = [North, South, East, West]

> nb  = [South, East, West]
> sb  = [North, East, West]
> eb  = [North, South, West]
> wb  = [North, South, East]
> nwc = [South, East]
> nec = [South, West]
> swc = [North, East]
> sec = [North, West]

> g0 :: Grid
> g0 = array ((-size,-size),(size,size))
>        ([ ((i, size),nb)   | i <- r ] ++
>         [ ((i,-size),sb)   | i <- r ] ++
>         [ (( size,i),eb)   | i <- r ] ++
>         [ ((-size,i),wb)   | i <- r ] ++
>         [ ((i,j),interior) | i <- r, j <- r ] ++
>         [ ((size,size), nec),((size,-size), sec),
>           ((-size,size),nwc),((-size,-size),swc)] )
>      where r = [1-size .. size-1]

< (//) :: Ix a => Array a b -> [(a,b)] -> Array a b

| colors // [(0,White) (7,Black)]

> mkHorWall, mkVerWall :: Int -> Int -> Int -> [(Position,[Direction])]

> mkHorWall x1 x2 y
>   = [ ((x,y),  nb) | x <- [x1..x2] ] ++
>     [ ((x,y+1),sb) | x <- [x1..x2] ]

> mkVerWall y1 y2 x
>   = [ ((x,y),  eb) | y <- [y1..y2] ] ++
>     [ ((x+1,y),wb) | y <- [y1..y2] ]

> g1 :: Grid
> g1 = g0 // mkHorWall (-5) 5 10

> mkBox :: Position -> Position -> [(Position,[Direction])]
> mkBox (x1,y1) (x2,y2)
>   = mkHorWall (x1+1) x2 y1 ++
>     mkHorWall (x1+1) x2 y2 ++
>     mkVerWall (y1+1) y2 x1 ++
>     mkVerWall (y1+1) y2 x2

< accum :: (Ix a) => (b->c->b) -> Array a b -> [(a,c)] -> Array a b

| [South, East, West] `intersect` [North, South, West]
| ===> [South, West]

> g2 :: Grid
> g2 = accum intersect g0 (mkBox (-15,8) (2,17))

> g3 :: Grid
> g3 = accum union g2 [((-7,17),interior),((-7,18),interior)]

> drawLine :: Window -> Color -> Point -> Point -> IO ()
> drawLine w c p1 p2
>   = drawInWindowNow w (withColor c (line p1 p2))

> d :: Int
> d = 5       -- half the distance between grid points

> wc, cc :: Color
> wc = Green  -- color of walls
> cc = Yellow -- color of coins

> xWin, yWin :: Int
> xWin = 600
> yWin = 500

> pause n =
>   do t0 <- timeGetTime
>      let loop = do t1 <- timeGetTime
>                    if (t1-t0) < n
>                       then loop
>                       else return ()
>      loop

> drawGrid :: Window -> Grid -> IO ()
> drawGrid w wld
>   = let (low@(xMin,yMin),hi@(xMax,yMax)) = bounds wld
>         (x1,y1) = trans low
>         (x2,y2) = trans hi
>     in do drawLine w wc (x1-d,y1+d) (x1-d,y2-d)
>           drawLine w wc (x1-d,y1+d) (x2+d,y1+d)
>           sequence_ [drawPos w (trans (x,y)) (wld `at` (x,y)) 
>                     | x <- [xMin..xMax], y <- [yMin..yMax]]

> drawPos :: Window -> Point -> [Direction] -> IO ()
> drawPos w (x,y) ds
>   = do if North `notElem` ds
>           then drawLine w wc (x-d,y-d) (x+d,y-d)
>           else return ()
>        if East `notElem` ds
>           then drawLine w wc (x+d,y-d) (x+d,y+d)
>           else return ()

> drawCoins :: Window -> RobotState -> IO ()
> drawCoins w s = mapM_ (drawCoin w) (treasure s)

> drawCoin :: Window -> Position -> IO ()
> drawCoin w p = let (x,y) = trans p
>                in drawInWindowNow w 
>                    (withColor cc (ellipse (x-5,y-1) (x-1,y-5)))

> eraseCoin :: Window -> Position -> IO ()
> eraseCoin w p = let (x,y) = trans p
>                 in drawInWindowNow w 
>                      (withColor Black (ellipse (x-5,y-1) (x-1,y-5)))

> graphicsMove :: Window -> RobotState -> Position -> IO ()
> graphicsMove w s newPos
>   = do if pen s then drawLine w (color s) (trans (position s)) 
>                                           (trans newPos)
>                 else return ()

> trans :: Position -> Point
> trans (x,y) = (div xWin 2 + 2*d*x, div yWin 2 - 2*d*y)

> spaceWait :: Window -> IO ()
> spaceWait w
>   = do k <- getKey w
>        if k==' ' then return ()
>                  else spaceWait w

> runRobot :: Robot () -> RobotState -> Grid -> IO ()
> runRobot (Robot sf) s g
>   = runGraphics $
>     do w <- openWindowEx "Robot World" (Just (0,0)) 
>               (Just (xWin,yWin)) drawBufferedGraphic
>        drawGrid w g
>        drawCoins w s
>        spaceWait w
>        sf s g w
>        spaceClose w

> s0 :: RobotState
> s0 = RobotState { position = (0,0)
>                 , pen      = False
>                 , color    = Blue
>                 , facing   = North
>                 , treasure = tr
>                 , pocket   = 0
>                 }

> tr :: [Position]
> tr = [(x,y) | x <- [-13,-11 .. 1], y <- [9,11 .. 15]]

> main = runRobot spiral s0 g0

| cond p (c1 >> c) (c2 >> c)  ===>  cond p c1 c2 >> c
| repeat p c  ===>  c >> while p c
| turnTo d >> direction  ===>  return d

> main1 = runRobot treasureHunt s0 g3

> treasureHunt :: Robot ()
> treasureHunt = do
>   penDown 
>   loop 1
>    where loop n =
>            cond blocked findDoor $
>              do turnRight
>                 moven n
>                 cond blocked findDoor $
>                   do turnRight
>                      moven n
>                      loop (n+1)

> findDoor :: Robot ()
> findDoor = do 
>   -- setPenColor Red
>   turnLeft
>   loop
>    where loop = do
>            wallFollowRight
>            cond doorOnRight
>              (do enterRoom
>                  getGold)
>              (do turnRight
>                  move
>                  loop)

> doorOnRight :: Robot Bool
> doorOnRight = do
>   move
>   b <- blockedRight
>   turnAround
>   move
>   turnAround
>   return b

> blockedRight :: Robot Bool
> blockedRight = do
>   turnRight
>   b <- blocked
>   turnLeft
>   return b

> turnAround :: Robot ()
> turnAround = do
>   turnRight
>   turnRight

> wallFollowRight :: Robot ()
> wallFollowRight =
>   cond1 blockedRight $
>     do move
>        wallFollowRight

> enterRoom :: Robot ()
> enterRoom = do
>   turnRight
>   move
>   turnLeft
>   moveToWall
>   turnAround

> getGold :: Robot ()
> getGold = do
>   getCoinsToWall
>   turnLeft
>   move
>   turnLeft
>   getCoinsToWall
>   turnRight
>   cond1 (isnt blocked) $
>     do move
>        turnRight
>        getGold

> main1book = runRobotBook treasureHunt s0 g3

> runRobotBook :: Robot () -> RobotState -> Grid -> IO ()
> runRobotBook (Robot sf) s g
>   = runGraphics $
>     do w <- openWindowEx "Robot World" (Just (0,0)) (Just (xWin,yWin))
>               drawBufferedGraphic
>        -- drawBackground w
>        drawGrid w g
>        drawCoins w s
>        spaceWait w
>        -- getKey w
>        -- GW.getEvent w
>        (s',()) <- sf s g w
>        -- getKey w
>        -- printState s'
>        -- spaceWait w
>        spaceClose w

> drawBackground w =
>   drawInWindowNow w
>       (withColor White
>          (polygon [(0,0),(xWin,0),(xWin,yWin),(0,yWin)]))

> spaceClose :: Window -> IO ()
> spaceClose w
>   = do k <- getKey w
>        if k==' ' || k == '\x0'
>           then closeWindow w
>           else spaceClose w


> printState :: RobotState -> IO ()
> printState s
>   = do putStrLn "Ending Robot State:"
>        putStrLn ("  Position:  " ++ show (position s))
>        putStrLn ("  Facing:    " ++ show (facing s))
>        putStrLn ("  Pen Down:  " ++ show (pen s))
>        putStrLn ("  Pen Color: " ++ show (color s))
>        putStrLn ("  Coins at:  " ++ show (treasure s))
>        putStrLn ("  In Pocket: " ++ show (pocket s))

