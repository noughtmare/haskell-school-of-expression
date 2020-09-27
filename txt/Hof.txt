This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> multSumByFive = simple 5

| multSumByFive a b 
| ==> (simple 5) a b 
| ==> simple 5 a b 
| ==> 5*(a+b)

| simple :: Float -> Float -> Float -> Float
| simple 5 :: Float -> Float -> Float
| simple 5 a :: Float -> Float
| simple 5 a b :: Float

< listSum, listProd :: [Float] -> Float
< listSum  xs        = foldl (+) 0 xs
< listProd xs        = foldl (*) 1 xs

> listSum, listProd :: [Float] -> Float
> listSum            = foldl (+) 0
> listProd           = foldl (*) 1

< and, or :: [Bool] -> Bool
< and xs   = foldr (&&) True  xs
< or  xs   = foldr (||) False xs

< and, or :: [Bool] -> Bool
< and      = foldr (&&) True
< or       = foldr (||) False

< reverse xs = foldl revOp [] xs
<   where revOp acc x = x : acc

< flip :: (a -> b -> c) -> (b -> a -> c)
< flip f x y = f y x

< revOp acc x = flip (:) acc x

< revOp = flip (:)

< reverse = foldl (flip (:)) []

< (Polygon pts) `containsS` p
<    = let leftOfList = map isLeftOfp
<                           (zip pts (tail pts ++ [head pts]))
<          isLeftOfp p' = isLeftOf p p'
<      in and leftOfList

> xs = [1,2,3] :: [Float]
> ys = map (+) xs

| applyEach [simple 2 2, (+3)] 5  ===>  [14, 8]

| applyAll [simple 2 2, (+3)] 5  ===>  20

> appendr, appendl :: [a] -> [a] -> [a]
> appendr = foldr (flip (++)) []
> appendl = foldl (flip (++)) []

< f1 y   = x+y
< f2 x   = x+y
< f3 x y = x+y

| let test x = x > epsilon
| in takeWhile test s

| takeWhile (> epsilon) s

< posInts :: [Integer] -> [Bool]
< posInts xs = map test xs
<              where test x = x>0

< posInts   :: [Integer] -> [Bool]
< posInts xs = map (>0) xs

> posInts :: [Integer] -> [Bool]
> posInts  = map (>0)

| (twice (+1)) 2 ==> 4

| power (+2) 5 1  ===>  11

| map (\x-> (x+1)/2) xs

| map (\(a,b)->a+b) xs

< map (\ (Rectangle s1 s2) -> True
<        (Ellipse r1 r2)   -> False) shapes

< simple x y z = x*(y+z)

> simple = \x y z -> x*(y+z)

| (x+)  ==>  \y -> x+y
| (+y)  ==>  \x -> x+y
| (+)   ==>  \x y -> x+y

| fix f = f (fix f)

| remainder    :: Integer -> Integer -> Integer
| remainder a b = if a<b then a
|                 else remainder (a-b) b

< (.)      :: (b->c) -> (a->b) -> a -> c
< (f . g) x = f (g x)

< totalCircleArea      :: [Float] -> Float
< totalCircleArea radii = listSum (map circleArea radii)

< totalCircleArea      :: [Float] -> Float
< totalCircleArea radii = listSum ((map circleArea) radii)

< totalCircleArea      :: [Float] -> Float
< totalCircleArea radii = (listSum . (map circleArea)) radii

> totalCircleArea :: [Float] -> Float
> totalCircleArea  = listSum . map circleArea

< totalSquareArea      :: [Float] -> Float
< totalSquareArea sides = listSum (map squareArea sides)

> totalSquareArea :: [Float] -> Float
> totalSquareArea  = listSum . map squareArea

> circleArea = id
> squareArea = id

< putStr  :: String -> IO ()
< putStr s = sequence_ (map putChar s)

< putStr :: String -> IO ()
< putStr = sequence_ . map putChar

> allOverZero, oneOverZero :: [Integer] -> Bool
> allOverZero               = and . posInts
> oneOverZero               = or  . posInts

< allOverZero, oneOverZero :: [Integer] -> Bool
< allOverZero               = and . map (>0)
< oneOverZero               = or  . map (>0)

| map (\x-> (x+1)/2) xs

| map f (map g xs)

| map (\x-> (x+1)/2) xs

