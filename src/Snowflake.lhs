This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module Snowflake where
> import SOE

> m = 81  :: Int -- multiple of 3 for triangle size
> x = 250 :: Int -- x and y coordinates of
> y = 250 :: Int --         center of snowflake
> colors = [ Magenta, Blue, Green, Red, Yellow ]

> snowflake :: Window -> IO ()
> snowflake w = do
>   drawTri w x y m 0 False -- draw first triangle w/flat top
>   flake   w x y m 0 True  -- begin recursion to complete job

> flake :: Window -> Int -> Int -> Int -> Int -> Bool -> IO ()
> flake w x y m c o = do
>   drawTri w x y m c o  -- draw second triangle
>   let c1 = (c+1)`mod`5 -- get next color
>   if (m<=3) then return ()  -- if too small, we're done
>      else do
>        flake w (x-2*m) (y-m) (m`div`3) c1 True  -- NW
>        flake w (x+2*m) (y-m) (m`div`3) c1 True  -- NE
>        flake w  x    (y+2*m) (m`div`3) c1 True  -- S
>        flake w (x-2*m) (y+m) (m`div`3) c1 False -- SW
>        flake w (x+2*m) (y+m) (m`div`3) c1 False -- SE
>        flake w  x    (y-2*m) (m`div`3) c1 False -- N

> drawTri :: Window -> Int -> Int -> Int -> Int -> Bool -> IO ()
> drawTri w x y m c o =
>   let d =  (3*m) `div` 2
>       ps = if o
>            then [(x,y-3*m),  (x-3*m,y+d), (x+3*m,y+d)] -- side at bottom
>            else [ (x,y+3*m), (x-3*m,y-d), (x+3*m,y-d)] -- side at top
>   in drawInWindow w
>        (withColor (colors !! c)
>           (polygon ps))

> main
>   = runGraphics (
>     do w <- openWindow "Snowflake Fractal" (500,500)
>        drawInWindow w (withColor White
>          (polygon [(0,0),(499,0),(499,499),(0,499)]))
>        snowflake w
>        spaceClose w
>     )

> spaceClose :: Window -> IO ()
> spaceClose w
>   = do k <- getKey w
>        if k==' ' || k == '\x0'
>           then closeWindow w
>           else spaceClose w



