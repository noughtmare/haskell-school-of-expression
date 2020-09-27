This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module Shape ( Shape (Rectangle, Ellipse, RtTriangle, Polygon),
>                Radius, Side, Vertex, 
>                square, circle, distBetween, area
>              ) where 

< import Shape

< data Shape = Circle Float
<	     | Square Float

< data Shape = Rectangle Float Float
<            | Ellipse Float Float
<            | RtTriangle Float Float
<            | Polygon [(Float,Float)]
<      deriving Show

> data Shape = Rectangle Side Side
>            | Ellipse Radius Radius
>            | RtTriangle Side Side
>            | Polygon [Vertex]
>      deriving Show
>
> type Radius = Float
> type Side   = Float
> type Vertex  = (Float,Float)

< type Side = Double

> square s = Rectangle s s

> circle r = Ellipse r r

< area :: Shape -> Float

< area (Rectangle s1 s2)  = s1*s2

< area (RtTriangle s1 s2) = s1*s2/2

< area (Ellipse r1 r2)  = pi*r1*r2

< area (Polygon (v1:v2:v3:vs))
<   = triArea v1 v2 v3 + area (Polygon (v1:v3:vs))
< area (Polygon _)
<   = 0

< area (Polygon (v1:vs)) = polyArea vs
<      where polyArea           :: [Vertex] -> Float
<            polyArea (v2:v3:vs') = triArea v1 v2 v3
<                                      + polyArea (v3:vs')
<            polyArea _           = 0

> triArea         :: Vertex -> Vertex -> Vertex -> Float
> triArea v1 v2 v3 = let a = distBetween v1 v2
>                        b = distBetween v2 v3
>                        c = distBetween v3 v1
>                        s = 0.5*(a+b+c)
>                    in sqrt (s*(s-a)*(s-b)*(s-c))

| let a = aLongName
|         + anEvenLongerName
|     b = 56
| in ...

| let a = aLongName
|   + anEvenLongerName
|     b = 56
| in ...

| let a = aLongName
|         + anEvenLongerName
|   b = 56
| in ...

> distBetween :: Vertex -> Vertex -> Float
> distBetween (x1,y1) (x2,y2) 
>   = sqrt ((x1-x2)^2 + (y1-y2)^2)

> area                   :: Shape -> Float
> area (Rectangle  s1 s2) = s1*s2
> area (RtTriangle s1 s2) = s1*s2/2
> area (Ellipse r1 r2)    = pi*r1*r2
> area (Polygon (v1:vs))  = polyArea vs
>      where polyArea               :: [Vertex] -> Float
>            polyArea (v2:v3:vs') = triArea v1 v2 v3
>                                      + polyArea (v3:vs')
>            polyArea _           = 0

| map area [ Rectangle 2 4, RtTriangle 3 5, Ellipse 4 6,
|            Polygon [(1,0),(2,1),(2,2),(1,3),(0,2),(0,1) ] ]

| area (circle r) 
| ==> { <<< unfold >>> circle <<< >>> }
| area (Ellipse r r) 
| ==> { <<< unfold >>> area <<< >>> }
| pi*r*r 
| ==> { <<< fold >>> (^) <<< >>> }
| pi*r^2
| ==> { <<< fold >>> circleArea <<< >>> }
| circleArea r

| area (RtTriangle s1 s2) 
| ===> area (Polygon [(0,0),(s1,0),(0,s2)])

| area (Polygon [(0,0),(s1,0),(0,s2)])
| ==> polyArea [(0,0),(s1,0),(0,s2)]
| ==> triArea (0,0) (s1,0) (0,s2) + polyArea [(0,0),(0,s2)]
| ==> triArea (0,0) (s1,0) (0,s2) + 0
| ==> let a = distBetween (0,0)  (s1,0)
|         b = distBetween (s1,0) (0,s2)
|         c = distBetween (0,s2) (0,0)
|         s = 0.5*(a+b+c)
|     in sqrt (s*(s-a)*(s-b)*(s-c))
| ==> let b = sqrt(s1^2+s2^2)
|         s = 0.5*(s1+b+s2)
|     in sqrt (s*(s-s1)*(s-b)*(s-s2))                        -- (1)

| s*(s-b) 
| ==> 0.5*(s1+b+s2)*(0.5*(s1+b+s2)-b)
| ==> 0.5*(s1+b+s2)*(0.5*(s1-b+s2))
| ==> 0.25*(s1+s2+b)*(s1+s2-b) 
| ==> 0.25*((s1+s2)^2-b^2)
| ==> 0.25*((s1+s2)^2-(s1^2+s2^2))
| ==> 0.25*(s1^2+2*s1*s2+s2^2-(s1^2+s2^2))
| ==> 0.25*(2*s1*s2) 
| ==> 0.5*s1*s2

| (s-s1)*(s-s2) 
| ==> (0.5*(s1+b+s2)-s1)*(0.5*(s1+b+s2)-s2)
| ==> 0.5*(b-s1+s2)*(0.5*(b+s1-s2))
| ==> 0.25*(b^2-(s1-s2)^2) 
| ==> 0.25*((s1^2+s2^2)-(s1-s2)^2)
| ==> 0.25*((s1^2+s2^2)-(s1^2-2*s1*s2+s2^2))
| ==> 0.5*s1*s2

| sqrt (s*(s-s1)*(s-b)*(s-s2))
| ==> sqrt ((0.5*s1*s2)*(0.5*s1*s2)) 
| ==> 0.5*s1*s2
| ==> area (RtTriangle s1 s2)

| area (RtTriangle 3.652 5.126)  ===>  9.36008
| area (Polygon [(0,0), (3.652,0), (0,5.126)])  ===>  9.36008

| area (RtTriangle 0.0001 5.126)  ===>  0.0002563
| area (Polygon [(0,0), (0.0001,0), (0,5.126)])  ===>  0.000256648

| area (Rectangle s1 s2) 
| ===> area (Polygon [(0,0),(s1,0),(s1,s2),(0,s2)])

< module Shape ( Shape (Rectangle, Ellipse, RtTriangle, Polygon),
<                Radius, Side, Vertex, 
<                square, circle, distBetween, area
<              ) where 
< ...

< module Shape where
< ...

