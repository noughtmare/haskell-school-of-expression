This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> (+) :: Num a => a -> a -> a

> (==) :: Eq a => a -> a -> Bool

| 42 == 42    ==>  True
| 42 == 43    ==>  False
| 'a' == 'a'  ==>  True
| 'a' == 'b'  ==>  False

| [42,43]  == [42,43]       ==>  True
| [4.2,4.3]  == [4.3,4.2]   ==>  False
| (42,'a') == (42,'a')      ==>  True

> x `elem`  []            = False
> x `elem` (y:ys)         = x==y || x `elem` ys

> elem :: Eq a => a -> [a] -> Bool

> elem :: a -> [a] -> Bool

> elem :: Integer -> [Integer] -> Bool

> square x = x*x

| square 42   ==>  1764
| square 4.2  ==>  17.64

> class Eq a where 
>   (==)                  :: a -> a -> Bool

> instance Eq Integer where 
>   x == y                =  IntegerEq x y

> instance Eq Float where
>   x == y                =  floatEq x y

> data Tree a = Leaf a | Branch (Tree a) (Tree a)

> instance Eq a => Eq (Tree a) where 
>   Leaf a       == Leaf b        =  a == b
>   Branch l1 r1 == Branch l2 r2  =  l1==l2 && r1==r2
>   _            == _             =  False

> class Eq a  where
>   (==), (/=)       :: a -> a -> Bool
>   x /= y           =  not (x == y)
>   x == y           =  not (x /= y)

< containsS :: Shape -> Point -> Bool
< containsR :: Region -> Point -> Bool

< class PC t where
<   contains :: t -> Point -> Bool

< instance PC Shape where
<   contains = containsS
<
< instance PC Region where
<   contains = containsR

> class  Eq a => Ord a  where
>   (<), (<=), (>=), (>)  :: a -> a -> Bool
>   max, min              :: a -> a -> a

> instance Ord a => Ord (Tree a)  where
>   Leaf _       <  Branch _ _    =  True
>   Leaf a       <  Leaf b        =  a < b
>   Branch  _  _ <  Leaf _        =  False
>   Branch l1 r1 <  Branch l2 r2  =  l1<l2 && r1<r2
>
>   t1           <= t2            =  t1<t2 || t1==t2
>   ...

> quicksort               ::  (Ord a) => [a] -> [a]

> class (Eq a, Show a) => C a where ...

> class C a where
>   m :: Eq b => a -> b

> class  (Eq a, Show a) => Num a  where
>   (+), (-), (*) :: a -> a -> a
>   negate :: a -> a
>   abs, signum :: a -> a
>   fromInteger :: Integer -> a

> intToFloat   :: Int -> Float
> intToFloat  n = fromInteger (toInteger n)

> data  Tree a  =  Leaf a | Branch (Tree a) (Tree a)  
>                  deriving Eq

> data  Tree a  =  Leaf a | Branch (Tree a) (Tree a)  
>                  deriving (Eq,Ord)

> data [a] = [] 
>          | a : [a] 
>      deriving (Eq, Ord)

> data Hello = Hello
>      deriving Show

| show Hello    ===>  "Hello"
| show (show Hello) ===> show "Hello"  ===>  "\"Hello\""
| show (show (show Hello)) ===> "\"\\\"Hello\\\"\""

> main = putStr (quine q)
> quine s = s ++ show s
> q = "main = putStr (quine q)\nquine s = s ++ show s\nq = "

> data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet
>      deriving (Eq,Enum,Show)

| show [Red..]  
| ===>  "[Red,Orange,Yellow,Green,Blue,Indigo,Violet]"

| (x /= y)  =  not (x == y)
| (x==y) && (y==z)  =:>  (x==z)

| a <= a
| (a <= b) && (b <= c) =:> (a <= c)
| (a <= b) && (b <= a) =:> (a == b)
| (a /= b) =:> (a < b) || (b < a)

> t1 = Branch (Leaf 1) (Leaf 3)
> t2 = Branch (Leaf 2) (Leaf 2)

> instance Ord a => Ord (Tree a)  where
>   Leaf _       <  Branch _ _                     =  True
>   Leaf a       <  Leaf b                         =  a < b
>   Branch  _  _ <  Leaf _                         =  False
>   Branch l1 r1 <  Branch l2 r2 | l1<l2           = True
>                                | l1==l2 && r1<r2 = True
>                                | otherwise       = False
>   ...

