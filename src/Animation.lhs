This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module Animation where
>
> import Shape
> import Draw
> import Picture
> import SOE hiding (Region)
> import qualified SOE as G (Region)

> -- import Word (word32ToInt)

> type Animation a = Time -> a
> type Time = Float

> rubberBall :: Animation Shape
> rubberBall t = Ellipse (sin t) (cos t)

> revolvingBall :: Animation Region
> revolvingBall t
>   = let ball = Shape (Ellipse 0.2 0.2) 
>     in Translate (sin t, cos t) ball

> planets :: Animation Picture
> planets t
>   = let p1 = Region Red (Shape (rubberBall t))
>         p2 = Region Yellow (revolvingBall t)
>     in p1 `Over` p2

> tellTime :: Animation String
> tellTime t = "The time is: " ++ show t

< animate :: String -> Animation Graphic -> IO ()

> main1 :: IO ()
> main1 = animate "Animated Shape" 
>          (withColor Blue . shapeToGraphic . rubberBall)

> main2 :: IO ()
> main2 = animate "Animated Text" (text (100,200) . tellTime)

< openWindowEx :: String -> Maybe Point -> Maybe Point ->
<                 (Graphic -> DrawFun) -> 
<                 IO Window

| w <- openWindowEx title (Just (x,y)) (Just (w,h)) 
|                   drawFun

> animate :: String -> Animation Graphic -> IO ()

> animate title anim
>   = runGraphics (
>     do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
>               drawBufferedGraphic
>        t0 <- timeGetTime
>        let loop =
>              do t <- timeGetTime
>                 let word32ToInt = fromInteger . toInteger
>                 let ft = intToFloat (word32ToInt (t-t0)) / 1000
>                 setGraphic w (anim ft)
>                 spaceCloseEx w loop
>        loop
>     )

< main1 = animate "Animated Shape" 
<          (withColor Blue . shapeToGraphic . rubberBall)
<
< main2 = animate "Animated Text" (text (100,200) . tellTime)

> regionToGraphic :: Region -> Graphic
> regionToGraphic = drawRegion . regionToGRegion

> main3 :: IO ()
> main3 = animate "Animated Region" 
>           (withColor Yellow . regionToGraphic . revolvingBall)

> picToGraphic :: Picture -> Graphic
> picToGraphic (Region c r)
>   = withColor c (regionToGraphic r)
> picToGraphic (p1 `Over` p2)
>   = picToGraphic p1 `overGraphic` picToGraphic p2
> picToGraphic EmptyPic 
>   = emptyGraphic

> main4 :: IO ()
> main4 = animate "Animated Picture" (picToGraphic . planets)

> dt = pi/8 :: Float

> main4book = 
>  foldl Over EmptyPic
>  [ let p1 = Region Red (move (Shape (rubberBall t)))
>        p2 = Region Blue (move (revolvingBall t))
>        move r = Translate (2*i,5/3*j) (Scale (2/3,2/3) r)
>        t  = dt*(4+i-3*j)
>    in p1 `Over` p2
>    | i <- [-1,0,1], j<-[1,0,-1] ]
>  `Over` Region White (Shape (Rectangle 6 5))

> emptyA :: Animation Picture
> emptyA t = EmptyPic

> overA :: Animation Picture -> Animation Picture -> Animation Picture
> overA a1 a2 t = a1 t `Over` a2 t

> overManyA :: [Animation Picture] -> Animation Picture
> overManyA = foldr overA emptyA

< instance Eq (Animation a) where ...

< instance Eq (Time -> a) where ...

> newtype Behavior a = Beh (Time -> a)

> animateB :: String -> Behavior Picture -> IO ()
> animateB s (Beh pf) = animate s (picToGraphic . pf)

> instance Eq (Behavior a) where
>   a1 == a2 = error "Can't compare behaviors."
>
> instance Show (Behavior a)  where
>   showsPrec n a1 = error "<< Behavior >>"
>
> instance Num a => Num (Behavior a) where
>   (+) = lift2 (+)
>   (*) = lift2 (*)
>   negate = lift1 negate
>   abs = lift1 abs
>   signum = lift1 signum
>   fromInteger = lift0 . fromInteger

> instance Fractional a => Fractional (Behavior a) where
>   (/) = lift2 (/)
>   fromRational = lift0 . fromRational

> instance Floating a => Floating (Behavior a) where
>   pi    = lift0 pi
>   sqrt  = lift1 sqrt
>   exp   = lift1 exp
>   log   = lift1 log
>   sin   = lift1 sin
>   cos   = lift1 cos
>   tan   = lift1 tan
>   asin  = lift1 asin
>   acos  = lift1 acos
>   atan  = lift1 atan
>   sinh  = lift1 sinh
>   cosh  = lift1 cosh
>   tanh  = lift1 tanh
>   asinh = lift1 asinh
>   acosh = lift1 acosh
>   atanh = lift1 atanh

> lift0 :: a -> Behavior a
> lift0 x = Beh (\t -> x)
>
> lift1 :: (a -> b) -> (Behavior a -> Behavior b)
> lift1 f (Beh a) 
>   = Beh (\t -> f (a t))
>
> lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
> lift2 g (Beh a) (Beh b) 
>   = Beh (\t -> g (a t) (b t))
>
> lift3 :: (a -> b -> c -> d) -> 
>          (Behavior a -> Behavior b -> Behavior c -> Behavior d)
> lift3 g (Beh a) (Beh b) (Beh c)
>   = Beh (\t -> g (a t) (b t) (c t))

> time :: Behavior Time
> time = Beh (\t -> t)

| time + 5
| ==> { <<< unfold overloadings for >>> time, (+), <<< and 5 >>> }
| (lift2 (+)) (Beh (\t -> t)) (Beh (\t -> 5))
| ==> { <<< unfold >>> lift2 <<< >>> }
| (\ (Beh a) (Beh b) -> Beh (\t -> a t + b t)) (Beh (\t -> t)) (Beh (\t -> 5))
| ==> { <<< unfold anonymous function >>> }
| Beh (\t -> (\t -> t) t + (\t -> 5) t )
| ==> { <<< unfold two anonymous functions >>> }
| Beh (\t -> t + 5)

> instance Combine [a] where
>   empty = []
>   over  = (++)

> instance Combine (Fun a) where
>   empty = Fun id
>   Fun a `over` Fun b = Fun (a . b)

> newtype Fun a = Fun (a->a)

> class Combine a where
>   empty :: a
>   over  :: a -> a -> a

> instance Combine Picture where
>   empty = EmptyPic
>   over  = Over

> instance Combine a => Combine (Behavior a) where
>   empty = lift0 empty
>   over  = lift2 over

| Combine a => Behavior a -> Behavior a -> Behavior a

| Combine a => a -> a -> a

> m :: Behavior Picture
> m = let a = lift0 (empty `over` p)
>     in a `over` empty
>
> p :: Picture
> p = empty

> overMany :: Combine a => [a] -> a
> overMany = foldr over empty

> reg    = lift2 Region
> shape  = lift1 Shape
> ell    = lift2 Ellipse
> red    = lift0 Red
> yellow = lift0 Yellow
> translate (Beh a1, Beh a2) (Beh r)
>        = Beh (\t -> Translate (a1 t, a2 t) (r t))

> revolvingBallB :: Behavior Picture
> revolvingBallB 
>   = let ball = shape (ell 0.2 0.2) 
>     in reg red (translate (sin time, cos time) ball)

> main5 :: IO ()
> main5 = animateB "Revolving Ball Behavior" revolvingBallB

> (>*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
> (>*) = lift2 (>)

> ifFun :: Bool -> a -> a -> a
> ifFun p c a = if p then c else a

> cond :: Behavior Bool -> Behavior a -> Behavior a -> Behavior a
> cond = lift3 ifFun

> flash :: Behavior Color
> flash = cond (sin time >* 0) red yellow

> timeTrans :: Behavior Time -> Behavior a -> Behavior a

< timeTrans (Beh f) (Beh a) = Beh (\t -> a (f t))

> timeTrans (Beh f) (Beh a) = Beh (a . f)

| timeTrans (2*time) anim

| timeTrans (5+time) anim `over` anim

| timeTrans (negate time) anim

> flashingBall :: Behavior Picture
> flashingBall 
>   = let ball = shape (ell 0.2 0.2) 
>     in reg (timeTrans (8*time) flash) 
>            (translate (sin time, cos time) ball)

> main6 :: IO ()
> main6 = animateB "Flashing Ball" flashingBall

> revolvingBalls :: Behavior Picture
> revolvingBalls 
>   = overMany [ timeTrans (lift0 (t*pi/4) + time) flashingBall
>              | t <- [0..7]]

> main7 :: IO ()
> main7 = animateB "Lots of Flashing Balls" revolvingBalls

> class Turnable a where
>   turn :: Float -> a -> a

> instance Turnable Picture where
>   turn theta (Region c r)   = Region c (turn theta r)
>   turn theta (p1 `Over` p2) = turn theta p1 `Over` turn theta p2
>   turn theta EmptyPic       = EmptyPic

> instance Turnable a => Turnable (Behavior a) where
>   turn theta (Beh b) = Beh (turn theta . b)

> rotate :: Float -> Coordinate -> Coordinate
> rotate theta (x,y) 
>   = (x*c+y*s, y*c-x*s) 
>     where (s,c) = (sin theta, cos theta)

> instance Turnable Shape where
>   turn theta (Polygon ps) = Polygon (map (rotate theta) ps)

> instance Turnable Region where
>   turn theta (Shape sh) = Shape (turn theta sh)

> main8 :: IO ()
> main8 
>   = do animateB "kaleido1 (close window for next demo)" kaleido1
>        animateB "kaleido2" kaleido2

< spectrum = [ c | c <- [ minBound .. ], c /= Black ]

< data Color 
<   = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White
<       deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

< spectrum = [ c | c <- [ Black .. ], c /= Black ]

< spectrum = [ c | c <- [ Blue .. ] ]

> slowTime = 0.1 * time

> kaleido :: Integer -> (Float -> Behavior Coordinate) 
>                    -> Behavior Picture
> kaleido n f =
>   lift2 turn (pi*sin slowTime) $
>   overMany (zipWith reg (map lift0 (cycle spectrum))
>                         (map (flip turn poly) rads) )
>   where
>    rads   = map (((2*pi / fromInteger n) *) . fromInteger) [0..n-1]
>    poly   = polyShapeAnim (map f rads)

> kaleido1 = kaleido 6 star
>  where star x = syncPair ( 2 * cos (v*c+l), 
>                            2 * abs (sin (slowTime*s - l)) )
>          where v     = lift0 x
>                l     = v * (slowTime + 1)
>                (s,c) = (sin l, cos l)

> kaleido2 = kaleido 9 star
>  where star x = syncPair ( 2 * abs (sin (v*a + slowTime)), 
>                            2 * abs (cos (  a + slowTime)) )
>          where v = lift0 x
>                a = v + slowTime * sin (v * slowTime)

> syncList :: [Behavior a] -> Behavior [a]
> syncList l = Beh (\t -> map (\(Beh f) -> f t) l)

> syncPair :: (Behavior a, Behavior b) -> Behavior (a,b)
> syncPair (Beh x, Beh y) = Beh (\t -> (x t, y t))

> -- Create an animated polygon from a list of point behaviors
> polyShapeAnim :: [Behavior Coordinate] -> Behavior Region
> polyShapeAnim = lift1 (Shape . Polygon) . syncList

> -- The interesting colors (assuming Black background)
> spectrum :: [Color]
> spectrum = [c | c <- [minBound ..], c /= Black]

> main8book t = 
>  let Beh f = kaleido1
>  in draw "Kaleidoscope Snapshots" (f t `Over` wbg)

> wbg = Region White (Shape (Rectangle 6 5))
> x = main8book

< instance Turnable Shape where
<   turn theta (Polygon ps) = Polygon (map (rotate theta) ps)

< instance Turnable Region where
<   turn theta (Shape sh) = Shape (turn theta sh)

< i x      = x
< k x y    = x
< s f g x = f x (g x)

< time    = Beh i
< lift0 x = Beh (K x)
< lifti f (Beh b1) ... (Beh bi)
<         = Beh (s h bi) where Beh h = liftj f b1 ... bj

> spaceCloseEx w loop
>   = do k <- maybeGetWindowEvent w
>        case k of
>          Just (Key c d) | c == ' ' && d -> closeWindow w
>          Just Closed -> closeWindow w
>          Nothing -> loop
>          _ -> spaceCloseEx w loop
