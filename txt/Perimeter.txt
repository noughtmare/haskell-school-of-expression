This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module Perimeter ( perimeter,
>                    module Shape
>                  ) where
> import Shape

< perimeter                   :: Shape -> Float

< perimeter (Rectangle  s1 s2) = 2*(s1+s2)
< perimeter (RtTriangle s1 s2) = s1 + s2 + sqrt(s1^2+s2^2)

> sides        :: [Vertex] -> [Side]
> sides  []    = []
> sides (v:vs) = aux v vs
>       where aux v1 (v2:vs') = distBetween v1 v2 : aux v2 vs'
>             aux vn  []      = distBetween vn v  : []

| zipWith (+) [1,2,3] [4,5,6] ==> [5,7,9]

< sides    :: [Vertex] -> [Side]
< sides vs = zipWith distBetween vs (tail vs ++ [head vs])

< perimeter (Polygon vs)      = foldl (+) 0 (sides vs)

> epsilon = 0.0001 :: Float

< e = sqrt (r1^2 - r2^2) / r1

> nextEl      :: Float -> Float -> Float -> Float
> nextEl e s i = s*(2*i-1)*(2*i-3)*(e^2) / (4*i^2)

| scanl (+) 0 [1,2,3]  ==>  [0,1,3,6]

< scanl            :: (a -> b -> a) -> a -> [b] -> [a]
< scanl f q  []    = q : []
< scanl f q (x:xs) = q : scanl f (f q x) xs

< s = let aux s i = nextEl e s i
<     in scanl aux (0.25*e^2) [2..]

< s = scanl (nextEl e) (0.25*e^2) [2..]

< takeWhile               :: (a -> Bool) -> [a] -> [a]
< takeWhile p []          =  []
< takeWhile p (x:xs) 
<             | p x       =  x : takeWhile p xs
<             | otherwise =  []

> data Bool = False | True

| let test x = x > epsilon
| in takeWhile test s

< perimeter (Ellipse r1 r2)
<   | r1 > r2   = ellipsePerim r1 r2
<   | otherwise = ellipsePerim r2 r1
<   where ellipsePerim r1 r2
<           = let e = sqrt (r1^2 - r2^2) / r1
<                 s = scanl aux (0.25*e^2) [2..]
<                 aux s i = nextEl e s i
<                 test x = x > epsilon
<                 sSum = foldl (+) 0 (takeWhile test s)
<             in 2*r1*pi*(1-sSum)

> perimeter                   :: Shape -> Float
> perimeter (Rectangle  s1 s2) = 2*(s1+s2)
> perimeter (RtTriangle s1 s2) = s1 + s2 + sqrt(s1^2+s2^2)
> perimeter (Polygon vs)       = foldl (+) 0 (sides vs)
> perimeter (Ellipse r1 r2)
>   | r1 > r2   = ellipsePerim r1 r2
>   | otherwise = ellipsePerim r2 r1
>   where ellipsePerim r1 r2
>             = let e    = sqrt (r1^2 - r2^2) / r1
>                   s    = scanl (nextEl e) (0.25*e^2) [2..]
>                   test x = x > epsilon
>                   sSum = foldl (+) 0 (takeWhile test s)
>               in 2*r1*pi*(1-sSum)

< module Shape   ( Shape (Rectangle, Ellipse, RtTriangle, Polygon),
<                  Radius, Side, Vertex,
<                  area, perimeter
<                ) where 
< ...

