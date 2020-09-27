This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

| 3*(9+5) 
| ==> 3*14 
| ==> 42

| 3*(9+5) 
| ==> 3*9 + 3*5 
| ==> 27 + 3*5 
| ==> 27+15 
| ==> 42

> simple x y z = x*(y+z)

| simple 3 9 5 
| ==> 3*(9+5)
| ==> 3*14
| ==> 42

| simple a b c 
| ==> a*(b+c) 
| ==> a*(c+b)
| ==> simple a c b

| simple a b c
| ==> { <<< unfold >>> }
| a*(b+c)
| ==> { <<< commutativity >>> }
| a*(c+b)
| ==> { <<< fold >>> }
| simple a c b

| simple (simple 2 3 4) 5 6

> x = x + 1

| x 
| ==> x + 1
| ==> (x + 1) + 1
| ==> ((x + 1) + 1) + 1
| ==> (((x + 1) + 1) + 1) + 1
| ...

>                          42 :: Integer
>                         'a' :: Char
>                     [1,2,3] :: [Integer]
>                     ('b',4) :: (Char,Integer)

> simple       :: Integer -> Integer -> Integer -> Integer
> simple x y z  = x*(y+z)

< (+) :: Integer -> Integer -> Integer

| [ (2,3), (4,5) ]
| [ 'z', 42 ]
| ( 'z', -42 )
| simple 'a' 'b' 'c'
| ( simple 1 2 3, simple )

> pi :: Float
> pi  = 3.14159

> x :: Float
> x  = f (a-b+2) + g y (a-b+2)

> c = a-b+2
> x = f c + g y c

> x = let c = a-b+2 
>     in f c + g y c

> c = 42
> x = let c = a-b+2
>     in  f c + g y c

> a = 42
> a = 43

> a = 42
> b = 43
> a = 44

> totalArea :: Float
> totalArea  = pi*r1^2 + pi*r2^2 + pi*r3^2

> circleArea   :: Float -> Float
> circleArea r  = pi*r^2
>
> totalArea     = circleArea r1 + circleArea r2 + circleArea r3

> totalArea    =  let circleArea r = pi*r^2
>                 in  circleArea r1 + circleArea r2 + circleArea r3

> listSum :: [Float] -> Float

> listSum [] = 0

> listSum (x:xs) = x + listSum xs

> listSum        :: [Float] -> Float
> listSum  []     = 0
> listSum (x:xs)  = x + listSum xs

| listSum [1,2,3] 
| ==> listSum (1:(2:(3:[])))
| ==> 1 + listSum (2:(3:[]))
| ==> 1 + (2 + listSum (3:[]))
| ==> 1 + (2 + (3 + listSum []))
| ==> 1 + (2 + (3 + 0))
| ==> 1 + (2 + 3)
| ==> 1 + 5
| ==> 6

> totalArea = listSum [circleArea r1, circleArea r2, circleArea r3]

| listSum [circleArea r1, circleArea r2, circleArea r3]
| ===> { <<< unfold >>> listSum <<< (four succesive times) >>> }
| circleArea r1 + circleArea r2 + circleArea r3 + 0
| ===> { <<< unfold >>> circleArea <<< (three places) >>> }
| pi*r1^2 + pi*r2^2 + pi*r3^2 + 0
| ==> { <<< simple arithmetic >>> }
| pi*r1^2 + pi*r2^2 + pi*r3^2

> totalArea =  pi*r1^2 + pi*r2^2 + pi*r3^2

> totalArea = listSum [circleArea r1, circleArea r2, circleArea r3]

| a*(b+c) ==> a*b + a*c

| 5*(-0.123456  +    0.123457) ==> 4.99189e-006
| 5*(-0.123456) + 5*(0.123457) ==> 5.00679e-006

> veryBigNumber :: Integer
> veryBigNumber = 43208345720348593219876512372134059

| i :: Int
| i = 1234567890

