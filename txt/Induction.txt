This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

| foldr (:) [] []  ==>  []

| foldr (:) [] (x:xs) 
| ==> x : foldr (:) [] xs
| ==> x : xs

| length ([] ++ ys) 
| ==> length ys 
| ==> 0 + length ys 
| ==> length [] + length ys

| length ((x:xs) ++ ys) 
| ==> length (x : (xs ++ ys))
| ==> 1 + length (xs ++ ys) 
| ==> 1 + (length xs + length ys)
| ==> (1 + length xs) + length ys
| ==> length (x:xs) + length ys

| transList ps = map trans ps

| transList []     = []
| transList (p:ps) = trans p : transList ps

| transList []
| ==> []
| ==> map trans []

| transList (p:ps)
| ==> trans p : transList ps
| ==> trans p : map trans ps
| ==> map trans (p:ps)

> listSum  []     = 0
> listSum (x:xs)  = x + listSum xs

| listSum []
| ==> 0
| ==> fold (+) 0 []

| listSum (x:xs)
| ==> x + listSum xs
| ==> x + fold (+) 0 xs
| ==> fold (+) 0 (x:xs)

> reverse1  []    = []
> reverse1 (x:xs) = reverse1 xs ++ [x]

> reverse2 xs = foldl (flip (:)) [] xs

| reverse1 [] 
| ==> []
| ==> foldl (flip (:)) [] []
| ==> reverse2 []

| reverse1 (x:xs)
| ==> reverse1 xs ++ [x]
| ==> reverse2 xs ++ [x]
| ==> foldl (flip (:)) [] xs ++ [x]
| ==> ???

| foldl (flip (:)) [] xs ++ [x]
| ==> foldl (flip (:)) [] (x:xs)

| ...
| ==> foldl (flip (:)) [] xs ++ [x]
| ==> foldl (flip (:)) [] (x:xs)
| ==> reverse2 (x:xs)

| foldl (flip (:)) ys xs ++ [y]
| ==> foldl (flip (:)) (ys++[y]) xs

| foldl (flip (:)) [] xs ++ [x]
| ==> { <<< property (2) >>> }
| foldl (flip (:)) ([]++[x]) xs
| ==> { <<< unfold >>> (++) <<< >>> }
| foldl (flip (:)) [x] xs
| ==> { <<< fold >>> (flip (:)) <<< >>> }
| foldl (flip (:)) (flip (:) [] x) xs
| ==> { <<< fold >>> foldl <<< >>> }
| foldl (flip (:)) [] (x:xs)

| foldl (flip (:)) ys [] ++ [y]
| ==> { <<< unfold >>> foldl <<< >>> }
| ys++[y]
| ==> { <<< fold >>> foldl <<< >>> }
| foldl (flip (:)) (ys++[y]) []

| foldl (flip (:)) ys (x:xs) ++ [y]
| ==> { <<< unfold >>> foldl <<< >>> }
| foldl (flip (:)) (flip (:) ys x) xs ++ [y]
| ==> { <<< unfold >>> flip <<< >>> }
| foldl (flip (:)) (x:ys) xs ++ [y]
| ==> { <<< induction hypothesis >>> }
| foldl (flip (:)) ((x:ys)++[y]) xs
| ==> { <<< unfold >>> (++) <<< >>> }
| foldl (flip (:)) (x:(ys++[y])) xs
| ==> { <<< fold >>> foldl <<< >>> }
| foldl (flip (:)) (ys++[y]) (x:xs)

| foldl (flip (:)) [] xs ++ [x]
| ==> foldl (flip (:)) [x] xs

| foldl (flip (:)) [] [] ++ [x]
| ==> { <<< unfold >>> foldl <<< >>> }
| []++[x]
| ==> { <<< unfold >>> (++) <<< >>> }
| [x]
| ==> { <<< fold >>> foldl <<< >>> }
| foldl (flip (:)) [x] []

| foldl (flip (:)) [] (x:xs) ++ [y]
| ==> { <<< unfold >>> foldl <<< >>> }
| foldl (flip (:)) (flip (:) [] x) xs ++ [y]
| ==> { <<< unfold >>> flip <<< >>> }
| foldl (flip (:)) [x] xs ++ [y]
| ==> ???

| map (\x->x) = \x->x
| map (f . g) = map f . map g
| map f . tail = tail . map f
| map f . reverse = reverse . map f
| map f . concat = concat . map (map f)
| map f (xs ++ ys) = map f xs ++ map f ys

| f . head = head . map f

| foldr op e xs = foldl op e xs

| x `op1` (y `op2` z) = (x `op1` y) `op2` z
| x `op1` e = e `op2` x

| foldr op1 e xs = foldl op2 e xs

| foldr op e xs = foldl (flip op) e (reverse xs)

| (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
| xs ++ [] = [] ++ xs = xs

| take n xs ++ drop n xs = xs
| take m . take n = take (min m n)
| drop m . drop n = drop (m + n)
| take m . drop n = drop n . take (m + n)

| drop m . take n = take (n - m) . drop m

| reverse (reverse xs) = xs
| head (reverse xs) = last xs
| last (reverse xs) = head xs

< const x y = x

> (&&)       :: Bool -> Bool -> Bool
> True  && x  = x
> False && _  = False

| f . head = head . map f

| f (head [])
| ==> f bottom

| head (map f [])
| ==> head []
| ==> bottom

| f (head (x:xs))
| ==> f x
| ==> head (f x : map f xs)
| ==> head (map f (x:xs))

< ifFun :: Bool -> a -> a -> a
< ifFun pred cons alt = if pred then cons else alt

> (^)  :: Integer -> Integer -> Integer
> x^0 = 1
> x^n = x * x^(n-1)

| x^(0+m) 
| ==> x^m 
| ==> 1 * (x^m) 
| ==> x^0 * x^m

| x^((n+1)+m) 
| ==> x * x^(n+m) 
| ==> x * (x^n * x^m)
| ==> (x * x^n) * x^m
| ==> x^(n+1) * x^m

> (^)            :: Integer -> Integer -> Integer
> x^0             = 1
> x^n | n<0       = error "negative exponent"
>     | otherwise = x * x^(n-1)

> (^)            :: Integer -> Integer -> Integer
> x^n | n<0       = error "negative exponent"
>     | otherwise = f x n
>     where f x 0 = 1
>           f x n = x * f x (n-1)

> (^!)            :: Integer -> Integer -> Integer
> x^!n | n<0       = error "negative exponent"
>      | otherwise = ff x n
>      where ff x n | n==0      = 1
>                   | even n    = ff (x*x) (n `quot` 2)
>                   | otherwise = x * ff x (n-1)

| f x 0 ==> 1 ==> ff x 0

| f x (n+1)
| ==> x * f x n
| ==> x * ff x n
| ==> ff x (n+1)

| f x (n+1)
| ==> x * f x n
| ==> x * ff x n

| x * ff x n
| ==> x * (x * ff x (n-1))

| ff x (n+1)
| ==> ff (x*x) ((n+1) `quot` 2)
| ==>  f (x*x) ((n+1) `quot` 2)

| f x (n+1) 
| ==> f x ((n+1) `quot` 2 + (n+1) `quot` 2)
| ==> f x ((n+1) `quot` 2) * f x ((n+1) `quot` 2)

| f x ((n+1) `quot` 2) * f x ((n+1) `quot` 2)
| ==> f (x*x) ((n+1) `quot` 2)
| ==> ff (x*x) ((n+1) `quot` 2)
| ==> ff x (n+1)

| f x 0 * f x 0
| ==> 1 * 1 
| ==> 1
| ==> f (x*x) 0

| f x (n+1) * f x (n+1)
| ==> (x * f x n) * (x * f x n)
| ==> (x*x) * (f x n * f x n)
| ==> (x*x) * f (x*x) n
| ==> f (x*x) (n+1)

< fac1 :: Integer -> Integer
< fac1 0 = 1
< fac1 n = n * fac1 (n-1)

< fac2 :: Integer -> Integer
< fac2 n = fac' n 1
< where fac' 0 x = x
<       fac' n x = fac' (n-1) (n*x)

> (^)              :: (Num a, Integral b) => a -> b -> a
> x ^ 0            =  1
> x ^ n | n > 0    =  f x (n-1) x
>                     where f _ 0 y = y
>                           f x n y = g x n  where
>                                     g x n | even n  = g (x*x) (n `quot` 2)
>                                           | otherwise = f x (n-1) (x*y)
> _ ^ _            = error "Prelude.^: negative exponent"

