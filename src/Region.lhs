This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module Region ( Region (Shape, Translate, Scale, Complement, 
>                         Union, Intersect, Xor, Empty),
>                 Coordinate,
>                 containsS, containsR, 
>                 module Shape
>               ) where
> import Shape

> infixr 5 `Union`
> infixr 6 `Intersect`

> -- A Region is either:
> data Region = Shape Shape		-- primitive shape
>             | Translate Vector Region -- translated region
>             | Scale      Vector Region -- scaled region
>             | Complement Region	-- inverse of region
>             | Region `Union` Region   -- union of regions
>             | Region `Intersect` Region -- intersection of regions
>             | Region `Xor` Region -- XOR of regions
>             | Empty                   -- empty region
>      deriving Show
>
> type Vector = (Float,Float)

| r1 `Union` r2 `Union` r3
| r1 `Intersect` r2 `Union` r3

< infixr 5 `Union`
< infixr 6 `Intersect`

| r1 `Union` (r2 `Union` r3)
| (r1 `Intersect` r2) `Union` r3

| let circle = Shape (Ellipse 0.5 0.5)
|     square = Shape (Rectangle 1 1)
| in (Scale (2,2) circle)
|    `Union` (Translate (1,0) square)
|    `Union` (Translate (-1,0) square)

> oneCircle   = Shape (Ellipse 1 1)
> manyCircles = [ Translate (x,0) oneCircle | x <- [0,2..] ]

| [ Translate (0,0) oneCircle, 
|   Translate (2,0) oneCircle, 
|   Translate (4,0) oneCircle, 
|   ...

| [ (x,y) | x <- [0,1,2], y <- ['a','b'] ]

| [ (0,'a'), (0,'b'), (1,'a'), (1,'b'), (2,'a'), (2,'b') ]

> fiveCircles = foldr Union Empty (take 5 manyCircles)

< take                   :: Int -> [a] -> [a]
< take 0 _               =  []
< take _ []              =  []
< take n (x:xs) | n > 0  =  x : take (n-1) xs
< take _     _           =  error 
<                            "PreludeList.take: negative argument"

> r1 `difference` r2 = r1 `Intersect` (Complement r2)

< containsS :: Shape -> Coordinate -> Bool

> type Coordinate = (Float,Float)

< (Rectangle s1 s2) `containsS` (x,y)
<    = let t1 = s1/2
<          t2 = s2/2
<      in -t1<=x && x<=t1 && -t2<=y && y<=t2

< (Ellipse r1 r2) `containsS` (x,y)
<    = (x/r1)^2 + (y/r2)^2 <= 1

< isLeftOf :: Coordinate -> Ray -> Bool
< (px,py) `isLeftOf` ((ax,ay),(bx,by))
<        = let (s,t) = (px-ax, py-ay)
<              (u,v) = (px-bx, py-by)
<          in  s*v >= t*u
<
< type Ray = (Coordinate, Coordinate)

< (Polygon pts) `containsS` p
<    = let leftOfList = map isLeftOfp
<                           (zip pts (tail pts ++ [head pts]))
<          isLeftOfp p' = isLeftOf p p'
<      in and leftOfList

< and, or :: [Bool] -> Bool
< and bs =  foldr (&&) True  bs
< or  bs =  foldr (||) False bs

< (RtTriangle s1 s2) `containsS` p
<    = (Polygon [(0,0),(s1,0),(0,s2)]) `containsS` p

> isLeftOf :: Coordinate -> Ray -> Bool
> (px,py) `isLeftOf` ((ax,ay),(bx,by))
>        = let (s,t) = (px-ax, py-ay)
>              (u,v) = (px-bx, py-by)
>          in  s*v >= t*u
>
> type Ray = (Coordinate, Coordinate)

< containsR :: Region -> Coordinate -> Bool

< (Shape s) `containsR` p = s `containsS` p

< (Translate (u,v) r) `containsR` (x,y)
<    = r `containsR` (x-u, y-v)

< (Scale (u,v) r) `containsR` (x,y)
<    = r `containsR` (x/u, y/v)

< (Complement r) `containsR` p = not (r `containsR` p)
< Empty          `containsR` p = False

< (r1 `Union` r2)     `containsR` p
<     = r1 `containsR` p || r2 `containsR` p
< (r1 `Intersect` r2) `containsR` p
<     = r1 `containsR` p && r2 `containsR` p

< containsR :: Region -> Coordinate -> Bool
<
< (Shape s) `containsR` p
<    = s `containsS` p
< (Translate (u,v) r) `containsR` (x,y)
<    = r `containsR` (x-u,y-v)
< (Scale (u,v) r) `containsR` (x,y)
<    = r `containsR` (x/u,y/v)
< (Complement r) `containsR` p 
<    = not (r `containsR` p)
< (r1 `Union` r2)     `containsR` p
<    = r1 `containsR` p || r2 `containsR` p
< (r1 `Intersect` r2) `containsR` p
<    = r1 `containsR` p && r2 `containsR` p
< Empty `containsR` p 
<    = False
<
< containsS :: Shape -> Coordinate -> Bool
<
< (Rectangle s1 s2) `containsS` (x,y)
<    = let t1 = s1/2
<          t2 = s2/2
<      in -t1<=x && x<=t1 && -t2<=y && y<=t2
< (Ellipse r1 r2) `containsS` (x,y)
<    = (x/r1)^2 + (y/r2)^2 <= 1
< (Polygon pts) `containsS` p
<    = let leftOfList = map (isLeftOf p) 
<                        (zip pts 
<                          (tail pts ++ [head pts]))
<      in and leftOfList
< (RtTriangle s1 s2) `containsS` p
<    = (Polygon [(0,0),(s1,0),(0,s2)]) `containsS` p

> containsR :: Region -> Coordinate -> Bool
>
> (Shape s) `containsR` p
>    = s `containsS` p
> (Translate (u,v) r) `containsR` (x,y)
>    = let p = (x-u,y-v) in r `containsR` p
> (Scale (u,v) r) `containsR` (x,y)
>    = let p = (x/u,y/v) in r `containsR` p
> (Complement r) `containsR` p 
>    = not (r `containsR` p)
> (r1 `Union` r2)     `containsR` p
>    = r1 `containsR` p || r2 `containsR` p
> (r1 `Intersect` r2) `containsR` p
>    = r1 `containsR` p && r2 `containsR` p
> (r1 `Xor` r2) `containsR` p
>    = let a = r1 `containsR` p
>          b = r2 `containsR` p
>      in (a || b) && not (a && b)
> Empty `containsR` p 
>    = False
>
> containsS :: Shape -> Coordinate -> Bool
>
> (Rectangle s1 s2) `containsS` (x,y)
>    = let t1 = s1/2; t2 = s2/2
>      in (-t1<=x) && (x<=t1) && (-t2<=y) && (y<=t2)
> (Ellipse r1 r2) `containsS` (x,y)
>    = (x/r1)^2 + (y/r2)^2 <= 1
> (Polygon pts) `containsS` p
>    = let leftOfList = map (isLeftOf p) 
>                           (zip pts (tail pts ++ [head pts]))
>      in and leftOfList
> (RtTriangle s1 s2) `containsS` p
>    = (Polygon [(0,0),(s1,0),(0,s2)]) `containsS` p

< ra = r1 `Union` (r2 `Union` r3)
< rb = (r1 `Union` r2) `Union` r3

< r1 `Union` (r2 `Union` r3) === (r1 `Union` r2) `Union` r3
< r1 `Intersect` (r2 `Intersect` r3) === (r1 `Intersect` r2) `Intersect` r3

| (r1 `Union` (r2 `Union` r3)) `containsR` p
| ==> (r1 `containsR` p) || ((r2 `Union` r3) `containsR` p)
| ==> (r1 `containsR` p) || ((r2 `containsR` p) || (r3 `containsR` p))
| ==> ((r1 `containsR` p) || (r2 `containsR` p)) || (r3 `containsR` p)
| ==> ((r1 `Union` r2) `containsR` p) || (r3 `containsR` p)
| ==> ((r1 `Union` r2) `Union` r3) `containsR` p

| r1 `Union` r2 === r2 `Union` r1
| r1 `Intersect` r2 === r2 `Intersect` r1

| (r1 `Union` r2) `containsR` p
| ==> (r1 `containsR` p) || (r2 `containsR` p)
| ==> (r2 `containsR` p) || (r1 `containsR` p)
| ==> (r2 `Union` r1) `containsR` p

> univ = Complement Empty

< r1 `Intersect` (r2 `Union` r3)
< === (r1 `Intersect` r2) `Union` (r1 `Intersect` r3)
<
< r1 `Union` (r2 `Intersect` r3)
< === (r1 `Union` r2) `Intersect` (r1 `Union` r3)

| r `Union`    Empty === r
| r `Intersect` univ === r

| r `Union` Complement r     === univ
| r `Intersect` Complement r === Empty

< r `Intersect` r === r
< r `Union`     r === r

< r `Union` univ      === univ
< r `Intersect` Empty === Empty

< r1 `Union` (r1 `Intersect` r2) === r1
< r1 `Intersect` (r1 `Union` r2) === r1

< Complement (Complement r) === r

< Complement Empty === univ
< Complement univ  === Empty

< Complement (r1 `Union` r2) 
<   === Complement r1 `Intersect` Complement r2
< Complement (r1 `Intersect` r2) 
<   === Complement r1 `Union` Complement r2

< rectangle :: Side -> Side -> Shape
< rectangle s1 s2 = 
<   | s1 >= 0 && s2 >= 0 = Rectangle s1 s2
<   | otherwise          = error "negative side lengths"

< rectangle :: Side -> Side -> Shape
< rectangle s1 s2 = Rectangle (abs s1) (abs s2)

< data Maybe a = Nothing | Just a

< rectangle :: Side -> Side -> Maybe Shape
< rectangle s1 s2 = 
<   | s1 >= 0 && s2 >= 0 = Just (Rectangle s1 s2)
<   | otherwise          = Nothing

| Rectangle s1 s2 `containsS` p = Rectangle (-s1) s2 `containsS` p
| Ellipse   r1 r2 `containsS` p = Ellipse   (-r1) r2 `containsS` p

