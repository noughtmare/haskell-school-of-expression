This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module StreamTest where
> import Memo1

> data Stream a = a :^ Stream a

> twos = 2 : twos

| twos
| ==> 2 : twos
| ==> 2 : 2 : twos
| ==> 2 : 2 : 2 : twos
| ...

< head      :: [a] -> a
< head (x:_) =  x

| head twos
| ==> head (2 : twos)
| ==> 2

| head twos
| head (2 : twos)
| head (2 : 2 : twos)
| head (2 : 2 : 2 : twos)
| ...

| head twos

| head (2 : twos)

| 2

> ignore :: a -> b -> b
> ignore a b = b

| 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...

> fib :: Integer -> Integer
> fib 0 = 1
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)

| fib 8
| ===> fib 7 + fib 6
| ===> (fib 6 + fib 5) + (fib 5 + fib 4)
| ===> ((fib 5 + fib 4) + (fib 4 + fib 3)) 
|      + ((fib 4 + fib 3) + (fib 3 + fib 2))
| ===> (((fib 4 + fib 3) + (fib 3 + fib 2)) 
|       + ((fib 3 + fib 2) + (fib 2 + fib 1)))
|      + (((fib 3 + fib 2) + (fib 2 + fib 1))
|        + ((fib 2 + fib 1) + (fib 1 + fib 0)))
| ...

> fibs :: [Integer]
> fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

| [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

| fibs
| ==> 1 : 1 : add fibs (tail fibs)

| ==> 1 : 1 : add (1 : 1 : add fibs (tail fibs)) 
|                 (1 : add fibs (tail fibs))

| fibs
| ==> 1 : 1 : add fibs (tail fibs)

| ==> 1 : tf
|     where tf = 1 : add fibs (tail fibs)
| ==> 1 : tf
|     where tf = 1 : add fibs tf

| ==> 1 : tf
|     where tf = 1 : tf2
|                where tf2 = add fibs tf

| ==> 1 : tf
|     where tf = 1 : tf2
|                where tf2 = 2 : add tf tf2

| ==> 1 : tf
|     where tf = 1 : tf2
|                where tf2 = 2 : tf3
|                            where tf3 = add tf tf2
| ==> 1 : tf
|     where tf = 1 : tf2
|                where tf2 = 2 : tf3
|                            where tf3 = 3 : add tf2 tf3

| ==> 1 : 1 : tf2
|     where tf2 = 2 : tf3
|                 where tf3 = 3 : add tf2 tf3

| ==> 1 : 1 : tf2
|     where tf2 = 2 : tf3
|                 where tf3 = 3 : tf4
|                             where tf4 = add tf2 tf3
| ==> 1 : 1 : tf2
|     where tf2 = 2 : tf3
|                 where tf3 = 3 : tf4
|                             where tf4 = 5 : add tf3 tf4
| ==> 1 : 1 : 2 : tf3
|                 where tf3 = 3 : tf4
|                             where tf4 = 5 : add tf3 tf4

> client :: [Response] -> [Request]
> server :: [Request]  -> [Response]

> reqs  = client resps
> resps = server reqs

> type Request  = Integer
> type Response = Integer

> client ys = 1 : ys
> server xs = map (+1) xs

| reqs
| ==> client resps
| ==> 1 : resps
| ==> 1 : server reqs

| ==> 1 : tr
|     where tr = server reqs
| ==> 1 : tr
|     where tr = 2 : server tr
| ==> 1 : tr
|     where tr = 2 : tr2
|           where tr2 = server tr
| ==> 1 : tr
|     where tr = 2 : tr2
|           where tr2 = 3 : server tr2
| ==> 1 : 2 : tr2
|     where tr2 = 3 : server tr2

| [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

> reqs1  = client1 resps1
> resps1 = server1 reqs1

> client1 (y:ys) = if ok y then 1 : (y:ys)
>                  else error "faulty server"
> server1 xs     = map (+1) xs

< client (y:ys) = if ok y then 1 : (y:ys)
<                 else error "faulty server"

> ok y = True

| reqs
| ==> client resps
| ==> client (server reqs)
| ==> client (server (client resps))
| ==> client (server (client (server reqs)))
| ...

< client ys = 1 : if ok (head ys) then ys
<                 else error "faulty server"

< client ~(y:ys) = 1 : if ok y then y:ys
<                      else error "faulty server"

> reqs2  = client2 resps2
> resps2 = server2 reqs2

> client2 ~(y:ys) = 1 : if ok y then y:ys
>                       else error "faulty server"
> server2 xs      = map (+1) xs

| reqs
| ==> client resps
| ==> 1 : if ok y then y:ys
|            else error "faulty server"
|     where y:ys = resps
| ==> 1 : (y:ys)
|     where y:ys = resps
| ==> 1 : resps

> fibs' :: [Integer]
> fibs'@(1:tfibs) = 1 : 1 : zipWith (+) fibs' tfibs

> fibsFn :: () -> [Integer]
> fibsFn x = 1 : 1 : zipWith (+) (fibsFn ()) (tail (fibsFn ()))

< data () = ()

| fibsFn ()
| ==> 1 : 1 : add (fibsFn ()) (tail (fibsFn ()))
| ==> 1 : tf
|     where tf = 1 : add (fibsFn ()) (tail (fibsFn ()))

| ==> 1 : tf
|     where tf = 1 : add (fibs()) tf

> fibsFn' :: Integer -> [Integer]
> fibsFn' x = 1 : 1 : zipWith (+) (fibsFn' 42) (tail (fibsFn' 43))

| ==> 1 : tf
|     where tf = 1 : add (fibsFn ()) 
|                        (tail (1 : 1 : add (fibsFn ()) (tail (fibsFn ()))))

< memo :: (a->b) -> (a->b)

< memo f x = f x

> mfibsFn :: () -> [Integer]
> mfibsFn x = let mfibs = memo1 mfibsFn
>             in 1 : 1 : zipWith (+) (mfibs ()) (tail (mfibs ()))

| take n xs ++ drop n xs = xs

| reverse (reverse xs) = xs

> bot :: a
> bot = bot

| bottom
| 1 : bottom
| 1 : 2 : bottom
| 1 : 2 : 3 : bottom
| 1 : 2 : 3 : 4 : bottom
| 1 : 2 : 3 : 4 : []

| bottom
| 1 : bottom
| 1 : 2 : bottom
| 1 : 2 : 3 : bottom
| 1 : 2 : 3 : 4 : bottom
| 1 : 2 : 3 : 4 : 5 : bottom
| 1 : 2 : 3 : 4 : 5 : 6 : bottom
| 1 : 2 : 3 : 4 : 5 : 6 : 7 : bottom
| ...

| take n xs ++ drop n xs = xs

< take                   :: Int -> [a] -> [a]
< take 0 _               =  []
< take _ []              =  []
< take n (x:xs) | n > 0  =  x : take (n-1) xs

< (++)        :: [a] -> [a] -> [a]
< []     ++ ys = ys
< (x:xs) ++ ys = x : (xs++ys)

| take n bottom ++ drop n bottom
| ==> bottom ++ drop n bottom
| ==> bottom

| reverse (reverse xs) = xs

| reverse (reverse bottom)
| ==> reverse bottom
| ==> bottom

| reverse (reverse (x:xs))
| ==> { <<< unfold >>> reverse <<< >>> }
| reverse (reverse xs ++ [x])
| ==> { <<< lemma (see below) >>> }
| reverse [x] ++ reverse (reverse xs)
| ==> { <<< unfold >>> reverse <<< >>> }
| [x] ++ reverse (reverse xs)
| ==> { <<< induction hypothesis >>> }
| [x] ++ xs
| ==> { <<< unfold >>> (++) <<< >>> }
| x : ([] ++ xs)
| ==> { <<< unfold >>> (++) <<< >>> }
| x : xs

| reverse (xs ++ ys) = reverse ys ++ reverse xs

| reverse (bottom ++ ys)
| ==> reverse bottom
| ==> bottom

| reverse ys ++ reverse bottom
| ==> reverse ys ++ bottom

> xs = 1 : map (*2) xs
> ys = [1] : [ zipWith (+) (0:q) (q ++ [0]) | q <- ys ]

< type Poly = [Float]

< pn :: Poly
< pn  = 1 : 0 : 2 : (-4) : 0 : 3 : repeat 0

< repeat           :: a -> [a]
< repeat x         =  xs where xs = x:xs

< scale :: Float -> Poly -> Poly
< scale a = map (*a)

