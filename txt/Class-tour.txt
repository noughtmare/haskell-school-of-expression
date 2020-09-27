This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> class  (Eq a) => Ord a  where
>     compare              :: a -> a -> Ordering
>     (<), (<=), (>=), (>) :: a -> a -> Bool
>     max, min             :: a -> a -> a
> 
>     compare x y
>          | x == y    =  EQ
>          | x <= y    =  LT
>          | otherwise =  GT
> 
>     x <= y           =  compare x y /= GT
>     x <  y           =  compare x y == LT
>     x >= y           =  compare x y /= LT
>     x >  y           =  compare x y == GT
> 
>     max x y 
>          | x >= y    =  x
>          | otherwise =  y
>     min x y
>          | x <  y    =  x
>          | otherwise =  y
> data  Ordering  =  LT | EQ | GT
>       deriving (Eq, Ord, Enum, Read, Show, Bounded)

> data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet

| [Red .. Violet]   ===>  [Red, Orange, Yellow, Green, Blue, Indigo, Violet]
| [Red, Yellow ..]  ===>  [Red, Yellow, Blue, Violet]
| fromEnum Green    ===>  3
| toEnum 5 :: Color ===>  Indigo

> class  Enum a  where
>     succ, pred       :: a -> a
>     toEnum           :: Int -> a
>     fromEnum         :: a -> Int
>     enumFrom         :: a -> [a]             -- [n..]
>     enumFromThen     :: a -> a -> [a]        -- [n,n'..]
>     enumFromTo       :: a -> a -> [a]        -- [n..m]
>     enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]
> 
>     -- Minimal complete definition: toEnum, fromEnum
>     succ             =  toEnum . (+1) . fromEnum
>     pred             =  toEnum . (subtract 1) . fromEnum
>     enumFrom x       =  map toEnum [fromEnum x ..]
>     enumFromThen x y =  map toEnum [fromEnum x, fromEnum y .. ]
>     enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
>     enumFromThenTo x y z = 
>                         map toEnum [fromEnum x, fromEnum y .. fromEnum z]

> class  Bounded a  where
>     minBound         :: a
>     maxBound         :: a

> show :: (Show a) => a -> String

| "The sum of " ++ show x ++ " and " ++ show y ++ " is " 
|      ++ show (x+y) ++ "."

| showTree (Branch (Branch (Leaf 2) (Leaf 3)) (Leaf 4))
| ===> "< <2|3>|4>"

> showTree             :: (Show a) => Tree a -> String
> showTree (Leaf x)     
>          =  show x
> showTree (Branch l r) 
>          = "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

> shows :: (Show a) => a -> String -> String

> showsTree               :: (Show a) => Tree a -> String -> String
> showsTree (Leaf x) s     
>      = shows x s
> showsTree (Branch l r) s 
>      = "<" ++ showsTree l ("|" ++ showsTree r (">" ++ s))

> showTree t  =  showsTree t ""

> type ShowS              =  String -> String

> showsTree               :: (Show a) => Tree a -> ShowS
> showsTree (Leaf x)      
>      =  shows x
> showsTree (Branch l r)  
>      =  ("<"++) . showsTree l . ("|"++) . showsTree r . (">"++)

> class  Show a  where
>     showsPrec :: Int -> a -> ShowS
>     showList  :: [a] -> ShowS

>     showList []       
>       = showString "[]"
>     showList (x:xs)   
>       = showChar '[' . shows x . showl xs
>         where showl []     = showChar ']'
>               showl (x:xs) = showString ", " . shows x . showl xs

> shows :: (Show a) => a -> ShowS
> shows  =  showsPrec 0
> 
> show   :: (Show a) => a -> String
> show x  =  shows x ""

> instance (Show a) => Show (Tree a) where
>     showsPrec n = showsTree

> type ReadS a            =  String -> [(a,String)]

> reads :: (Read a) => ReadS a

> readsTree        :: (Read a) => ReadS (Tree a)
> readsTree ('<':s) =  [(Branch l r, u) | (l, '|':t) <- readsTree s,
>                                         (r, '>':u) <- readsTree t ]
> readsTree s       =  [(Leaf x, t)     | (x,t)      <- reads s]

| (reads "5 golden rings") :: [(Int,String)]
| ===> [(5, " golden rings")]

| readsTree "< <1|2>|3>"
| ===>  [(Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 1)), "")]

| readsTree "<1|2"  ===>  []

> lex :: ReadS String

> readsTree   :: (Read a) => ReadS (Tree a)
> readsTree s  =  [(Branch l r, x) | ("<", t) <- lex s,
>                                    (l,   u) <- readsTree t,
>                                    ("|", v) <- lex u,
>                                    (r,   w) <- readsTree v,
>                                    (">", x) <- lex w        ]
>                 ++
>                 [(Leaf x, t)     | (x,   t) <- reads s      ]

> class  Read a  where
>     readsPrec :: Int -> ReadS a
>     readList  :: ReadS [a]
> 
>     readList   = readParen False (\r -> [pr | ("[",s)  <- lex r,
>                                               pr       <- readl s])
>                      where readl  s = [([],t)   | ("]",t)  <- lex s] ++
>                                       [(x:xs,u) | (x,t)    <- reads s,
>                                                   (xs,u)   <- readl' t]
>                            readl' s = [([],t)   | ("]",t)  <- lex s] ++
>                                       [(x:xs,v) | (",",t)  <- lex s,
>                                                   (x,u)    <- reads t,
>                                                   (xs,v)   <- readl' u]
>
> readParen        :: Bool -> ReadS a -> ReadS a
> readParen b g    =  if b then mandatory else optional
>                     where optional r  = g r ++ mandatory r
>                           mandatory r = [(x,u) | ("(",s) <- lex r,
>                                                  (x,t)   <- optional s,
>                                                  (")",u) <- lex t    ]

> reads :: (Read a) => ReadS a
> reads  =  readsPrec 0
> 
> read   :: (Read a) => String -> a
> read s  =  case [x | (x,t) <- reads s, ("","") <- lex t] of
>              [x] -> x
>              []  -> error "PreludeText.read: no parse"
>              _   -> error "PreludeText.read: ambiguous parse"

> class  (Ord a) => Ix a  where
>     range       :: (a,a) -> [a]
>     index       :: (a,a) -> a -> Int
>     inRange     :: (a,a) -> a -> Bool

| range (0,4) ===> [0,1,2,3,4]
| range ((0,0),(1,2)) ===> [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]

| index (1,9) 2  ===>  1
| index ((0,0),(1,2)) (1,1)  ===>  4

> class  (Eq a, Show a) => Num a  where
>     (+), (-), (*) :: a -> a -> a
>     negate :: a -> a
>     abs, signum :: a -> a
>     fromInteger :: Integer -> a

> class  (Num a, Ord a) => Real a  where
>     toRational ::  a -> Rational

> class  (Real a, Enum a) => Integral a  where
>     quot, rem, div, mod :: a -> a -> a
>     quotRem, divMod :: a -> a -> (a,a)
>     toInteger :: a -> Integer

> class  (Num a) => Fractional a  where
>     (/) :: a -> a -> a
>     recip :: a -> a
>     fromRational :: Rational -> a

> class  (Fractional a) => Floating a  where
>     pi :: a
>     exp, log, sqrt :: a -> a
>     (**), logBase :: a -> a -> a
>     sin, cos, tan :: a -> a
>     asin, acos, atan :: a -> a
>     sinh, cosh, tanh :: a -> a
>     asinh, acosh, atanh :: a -> a

> class  (Real a, Fractional a) => RealFrac a  where
>     properFraction :: (Integral b) => a -> (b,a)
>     truncate, round :: (Integral b) => a -> b
>     ceiling, floor :: (Integral b) => a -> b

> class  (RealFrac a, Floating a) => RealFloat a  where
>     floatRadix :: a -> Integer
>     floatDigits :: a -> Int
>     floatRange :: a -> (Int,Int)
>     decodeFloat :: a -> (Integer,Int)
>     encodeFloat :: Integer -> Int -> a
>     exponent :: a -> Int
>     significand :: a -> a
>     scaleFloat :: Int -> a -> a
>     isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE 
>                         :: a -> Bool

