This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

> instance Functor Tree where
>   fmap f (Leaf x)       = Leaf   (f x)
>   fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)

> instance Functor Tree where
>   fmap = mapTree

> instance Functor [] where
>   fmap f []     = []
>   fmap f (x:xs) = f x : fmap f xs

> instance Functor [] where
>   fmap = map

| fmap id = id
| fmap (f . g) = fmap f . fmap g

> class  Monad m  where
>     (>>=)            :: m a -> (a -> m b) -> m b
>     (>>)             :: m a -> m b -> m b
>     return           :: a -> m a
>     fail             :: String -> m a
>
>     m >> k           =  m >>= \_ -> k
>     fail s           = error s

> do e ==> e

| do e1; e2; ...; en
| ==> e1 >> do e2 ; ...; en

| do writeFile "testFile.txt" "Hello File System"
|    putStr "Hello World"

| writeFile "testFile.txt" "Hello File System" >>
| putStr "Hello World"

> (>>) :: Monad m => m a -> m b -> m b

> (>>) :: IO () -> IO () -> IO ()

> do pat <- e1 ; e2 ; ...; en
> ==> let ok pat = do e2 ; ...; en
>         ok _   = fail "..."
>     in e1 >>= ok 

> do x <- e1 ; e2 ; ...; en
> ==> e1 >>= \x -> do e2 ; ...; en

> do let decllist ; e2 ; ...; en
> ==> let decllist in do e2 ; ...; en

< return a >>= k           = k a
< m >>= return             = m
< m >>= (\x -> k x >>= h)  = (m >>= k) >>= h

< m1 >> (m2 >> m3)  = (m1 >> m2) >> m3

< fmap f xs = xs >>= return . f

< fmap f xs                  = do x <- xs ; return (f x)

< do x <- return a ; k x     =  k a
< do x <- m ; return x       =  m
< do x <- m ; y <- k x ; h y = do y <- (do x <- m ; k x) ; h y
< do m1 ; m2 ; m3            = do (do m1 ; m2) ; m3

| do k <- getKey w
|    return k

< do k <- getKey w
<    n <- changeKey k
<    respond n

| let keyStuff = do k <- getKey w
|                   changeKey k
| in do n <- keyStuff
|       respond n

> instance  Monad Maybe  where
>     Just x  >>= k   =  k x
>     Nothing >>= k   =  Nothing
>     return          =  Just
>     fail s          =  Nothing

> instance  Functor Maybe  where
>     fmap f Nothing    =  Nothing
>     fmap f (Just x)   =  Just (f x)

| (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
| return :: a -> Maybe a

| g (f x)

| case (f x) of 
|   Nothing -> Nothing
|   Just y  -> case (g y) of
|                Nothing -> Nothing
|                Just z  -> z

| f x >>= \y ->
| g y >>= \z ->
| return z

| do y <- f x
|    z <- g y
|    return z

| f x >>= \y ->
| g y >>= \z ->
| return z
| ==> { <<< currying simplification >>> }
| f x >>= \y ->
| g y >>= return 
| ==> { <<< monad law for >>> return <<< >>> }
| f x >>= \y ->
| g y
| ==> { <<< currying simplification >>> }
| f x >>= g

> composeM :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
> (g `composeM` f) x = f x >>= g

> instance  Monad []  where
>     m >>= k          =  concat (map k m)
>     return x         =  [x]
>     fail x           =  [ ]

< concat :: [[a]] -> [a]
< concat xss = foldr (++) [] xss

| (>>=)  :: [a] -> (b -> [b]) -> [b]
| return :: a -> [a]

| do x <- [1,2,3]
|    y <- [4,5,6]
|    return (x,y)

| [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

| [(x,y) | x <- [1,2,3], y <- [4,5,6]]

< do x <- xs ; return (f x)

< [ f x | x <- xs ]

< fmap f xs = do x <- xs ; return (f x)

< data Id a = Id a

< putStr  :: String -> IO ()
< putStr s = sequence_ (map putChar s)

< putStr  :: String -> IO ()
< putStr s = mapM_ putChar s

< sequence       :: Monad m => [m a] -> m [a] 
< sequence       =  foldr mcons (return [])
<                     where mcons p q = do x  <- p
<                                          xs <- q
<                                          return (x:xs)

< sequence_      :: Monad m => [m a] -> m () 
< sequence_      =  foldr (>>) (return ())

< mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
< mapM f as        =  sequence (map f as)

< mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
< mapM_ f as       =  sequence_ (map f as)

< (=<<)            :: Monad m => (a -> m b) -> m a -> m b
< f =<< x          =  x >>= f

> class  Monad m => MonadPlus m  where
>      mzero  :: m a
>      mplus  :: m a -> m a -> m a

< m >>= (\x -> mzero) = mzero
< mzero >>= m         = mzero

> m `mplus` mzero = m
> mzero `mplus` m = m

> instance  MonadPlus Maybe  where
>     mzero                 = Nothing
>     Nothing `mplus` ys    = ys
>     xs      `mplus` ys    = xs

> instance  MonadPlus []  where
>     mzero = []
>     mplus = (++)

> data SM a = SM (S -> (S,a))

> instance Monad SM where
>   return a    
>     = SM (\s -> (s,a))
>   SM sm0 >>= fsm1
>     = SM $ \s0 ->
>         let (s1,a1) =  sm0 s0
>             SM sm1  = fsm1 a1
>             (s2,a2) =  sm1 s1
>         in (s2,a2)

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>      deriving Show

> test = let t = Branch (Leaf 'a') (Leaf 'b')
>        in label (Branch t t)

| Branch (Branch (Leaf 0) (Leaf 1)) 
|        (Branch (Leaf 2) (Leaf 3))

> label :: Tree a -> Tree Integer
> label t = snd (lab t 0)

> lab :: Tree a -> Integer -> (Integer, Tree Integer)
> lab (Leaf a) n 
>     = (n+1, Leaf n)
> lab (Branch t1 t2) n
>     = let (n1,t1') = lab t1  n
>           (n2,t2') = lab t2 n1
>       in  (n2, Branch t1' t2')

> newtype Label a = Label (Integer -> (Integer,a))

> instance Monad Label where
>   return a         
>     = Label (\s -> (s,a))
>   Label lt0 >>= flt1
>     = Label $ \s0 ->
>         let (s1,a1)   = lt0 s0
>             Label lt1 = flt1 a1
>         in lt1 s1

> mlabel :: Tree a -> Tree Integer
> mlabel t = let Label lt = mlab t
>            in snd (lt 0)

> mlab :: Tree a -> Label (Tree Integer)
> mlab (Leaf a)
>      = do n <- getLabel
>           return (Leaf n)
> mlab (Branch t1 t2)
>      = do t1' <- mlab t1
>           t2' <- mlab t2
>           return (Branch t1' t2')

> getLabel :: Label Integer
> getLabel = Label (\n -> (n+1,n))

> mtest = let t = Branch (Leaf 'a') (Leaf 'b')
>         in mlabel (Branch t t)

> instance Functor Int where ...

> instance Functor (Tree Int) where ...

