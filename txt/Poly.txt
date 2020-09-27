This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> length  []    = 0
> length (x:xs) = 1 + length xs

> length :: [a] -> Integer

> head             :: [a] -> a
> head (x:_)       =  x
>
> tail             :: [a] -> [a]
> tail (_:xs)      =  xs

> transList       :: [Vertex] -> [Point]
> transList []     = []
> transList (p:ps) = trans p : transList ps

> putCharList       :: String -> [IO ()]
> putCharList []     = []
> putCharList (c:cs) = putChar c : putCharList cs

> map f  []     = []
> map f (x:xs)  = f x : map f xs

> transList     :: [Vertex] -> [Point]
> transList  ps  = map trans ps
>
> putCharList   :: String -> [IO ()]
> putCharList cs = map putChar cs

< putStr  :: String -> IO ()
< putStr s = sequence_ (putCharList s)
<
< putCharList       :: String -> [IO ()]
< putCharList []     = []
< putCharList (c:cs) = putChar c : putCharList cs

< putStr  :: String -> IO ()
< putStr s = sequence_ (map putChar s)

< map :: (Vertex -> Point) -> [Vertex] -> [Point]

< map :: (Char -> IO ()) -> [Char] -> [IO ()]

< map :: (a -> b) -> [a] -> [b]

| (a->b) -> [a] -> [b]
| (Integer->b) -> [Integer] -> [b]
| (a->Float) -> [a] -> [Float]
| (Char->Char) -> [Char] -> [Char]

> totalArea = listSum [circleArea r1, circleArea r2, circleArea r3]

< totalArea = listSum (map circleArea [r1,r2,r3])

| map circleArea [r1, r2, r3]  
| ==> circleArea r1 : map circleArea [r2, r3]
| ==> circleArea r1 : circleArea r2 : map circleArea [r3]
| ==> circleArea r1 : circleArea r2 : circleArea r3 : map circleArea []
| ==> circleArea r1 : circleArea r2 : circleArea r3 : []
| ==> [circleArea r1, circleArea r2, circleArea r3]

> type ColoredShapes = [(Color,Shape)]

> drawShapes :: Window -> ColoredShapes -> IO ()
> drawShapes w [] 
>   = return ()
> drawShapes w ((c,s):cs)
>   = do draw w (withColor c (shapeToGraphic s))
>        drawShapes w cs

> drawShapes w css
>   = sequence_ (map aux css)
>     where aux (c,s) = draw w (withColor c (shapeToGraphic s))

> conCircles = map circle [2.4,2.1 .. 0.3]

> coloredCircles = 
>   zip [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White]
>       conCircles

| zip [1,2,3] [4,5,6] ==> [(1,4),(2,5),(3,6)]

< zip :: (a->b->c) -> [a]->[b]->[c]
< zip (a:as) (b:bs) = (a,b) : zip as bs
< zip  _      _    =  []

> main
>   = runGraphics (
>     do w <- openWindow "Bull's Eye" (xWin,yWin)
>        drawShapes w coloredCircles
>        spaceClose w
>     )

> (++) :: [a] -> [a] -> [a]

| [1,2,3] ++ [4,5,6]   ===>  [1,2,3,4,5,6]
| "hello" ++ " world"  ===>  "hello world"

> [] ++ ys = ys

> (x:xs) ++ ys = x : (xs++ys)

> (++)        :: [a] -> [a] -> [a]
> []     ++ ys = ys
> (x:xs) ++ ys = x : (xs++ys)

> xs ++ [] = xs

> xs ++ (y:ys) = ??

| (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

| [1,2,3] ++ xs
| ==> 1 : ([2,3] ++ xs)
| ==> 1 : 2 : ([3] ++ xs)
| ==> 1 : 2 : 3 : ([] ++ xs)
| ==> 1 : 2 : 3 : xs

| 2 * length xs + length ys

< infixr 5 ++

> listSum        :: [Float] -> Float
> listSum  []     = 0
> listSum (x:xs)  = x + listSum xs

> listProd        :: [Float] -> Float
> listProd  []     = 1
> listProd (x:xs)  = x * listProd xs

> fold op init  []     = init
> fold op init (x:xs)  = x `op` fold op init xs

> fold :: (Float->Float->Float) -> Float -> [Float] -> Float

> listSum, listProd :: [Float] -> Float
> listSum  xs        = fold (+) 0 xs
> listProd xs        = fold (*) 1 xs

> fold :: (a->b->b) -> b -> [a] -> b

> foldr                :: (a->b->b) -> b -> [a] -> b
> foldr op init  []     = init
> foldr op init (x:xs)  = x `op` foldr op init xs

| foldr op init (x1 : x2 : ... : xn : [])  
| ===>  x1 `op` (x2 `op` (...(xn `op` init)...))

| foldr (:) [] xs  ===>  xs

> foldl                :: (b->a->b) -> b -> [a] -> b
> foldl op init  []     = init
> foldl op init (x:xs)  = foldl op (init `op` x) xs

| foldl op init (x1 : x2 : ... : xn : [])
| ===>  (...((init `op` x1) `op` x2)...) `op` xn

> listSum, listProd :: [Integer] -> Integer
> listSum  xs = foldl (+) 0 xs
> listProd xs = foldl (*) 1 xs

< concat :: [[a]] -> [a]
< concat xss = foldr (++) [] xss

| concat [[1],[3,4],[],[5,6]]
| ==> [1,2,3,4,5,6]

| concat [xs1,xs2,...,xsn]
| ==> foldr (++) [] [xs1,xs2,...,xsn]
| ==> xs1 ++ (xs2 ++ ( ... (xn ++ [])) ... )

< slowConcat xss = foldl (++) [] xss

| slowConcat [xs1,xs2,...,xsn]
| ==> foldl (++) [] [xs1,xs2,...,xsn]
| ==> ( ... (([] ++ x1) ++ x2) ... ) ++ xn

| len + (len+len) + (len+len+len) + ... + (n-1)*len
| ==> n*(n-1)*len

> reverse       :: [a] -> [a]
> reverse  []    = []
> reverse (x:xs) = reverse xs ++ [x]

> reverse xs = rev [] xs
>    where rev acc  []    = acc
>          rev acc (x:xs) = rev (x:acc) xs

> foldl op init  []     = init
> foldl op init (x:xs)  = foldl op (init `op` x) xs

> rev op acc  []    = acc
> rev op acc (x:xs) = rev op (acc `op` x) xs

> revOp a b = b : a

| acc `revOp` x  ==>  revOp acc x  ==> x : acc

> reverse xs = rev revOp [] xs
>    where rev op acc  []    = acc
>          rev op acc (x:xs) = rev op (acc `op` x) xs

> reverse xs = foldl revOp [] xs

< area (Rectangle s1 s2) 
<   | s1 >= 0 && s2 >= 0 = s1*s2
<   | otherwise          = error "area: negative side lengths"

> (&&), (||)       :: Bool -> Bool -> Bool
> True  && x       =  x
> False && _       =  False
> True  || _       =  True
> False || x       =  x

| map map
| foldl foldl
| map foldl

| f1 (f2 (*) [1, 2, 3, 4]) 5 ==>  [5, 10, 15, 20]

| doubleEach [1,2,3] ===> [2,4,6]

| pairAndOne [1,2,3] ===> [(1,2),(2,3),(3,4)]

| addEachPair [(1,2),(3,4),(5,6)] ===> [3,7,11]

| addPairsPointwise [(1,2),(3,4),(5,6)] ===> (9,12)

| makeChange 99 [5,1] ==> [19,4]

