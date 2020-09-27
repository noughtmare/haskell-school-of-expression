This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> contrived :: ([a], Char, (Int, Float), String, Bool) -> Bool
> contrived ([],  'b',  (1,   2.0),   "hi",   True) 
>    = False

> f (x:xs)                = x:x:xs

> f s@(x:xs) = x:s

> head (x:_)             = x
> tail (_:xs)            = xs

> sign x |  x >  0        =   1
>        |  x == 0        =   0
>        |  x <  0        =  -1

> take  0     _           =  []
> take  _     []          =  []
> take  n     (x:xs)      =  x : take (n-1) xs

> take1  _     []         =  []
> take1  0     _          =  []
> take1  n    (x:xs)      =  x : take1 (n-1) xs

> take m ys               = case (m,ys) of
>                             (0,_)       ->  []
>                             (_,[])      ->  []
>                             (n,x:xs)    ->  x : take (n-1) xs

