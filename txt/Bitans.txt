This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> data Char       = 'a' | 'b' | 'c' | ...         -- This is not valid
>                 | 'A' | 'B' | 'C' | ...         -- Haskell code!
>                 | '1' | '2' | '3' | ...

> data Char       = Ca | Cb | Cc | ...
>                 | CA | CB | CC | ...
>                 | C1 | C2 | C3 | ...

>                -- more pseudo-code:
> data Int     = (-2^29) | ... | -1 | 0 | 1 | ... | (2^29-1)
> data Integer =       ... -2 | -1 | 0 | 1 | 2 ...

> data () = ()		-- more pseudo-code

> data (a,b)              = (a,b)      -- more pseudo-code
> data (a,b,c)            = (a,b,c)
> data (a,b,c,d)          = (a,b,c,d)

> data [a]               = [] | a : [a]                  -- more pseudo-code
> infixr 5 :

