This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module SimpleGraphics where

> import SOE

< module Main where
< main = putStr "Hello World\n"

| do writeFile "testFile.txt" "Hello File System"
|    putStr "Hello World\n"

< writeFile :: FilePath -> String -> IO ()
< type FilePath = String

< getLine :: IO String

| do s <- getLine
|    writeFile "testFile.txt" s

| do s <- readFile "testFile.txt"
|    putStr s

< actionList = [putStr "Hello World\n",
<               writeFile "testFile.txt" "Hello File System",
<               putStr "File successfully written."]

< sequence_ :: [IO a] -> IO ()

< main = sequence_ actionList

< type String = [Char]

< putCharList       :: String -> [IO ()]
< putCharList []     = []
< putCharList (c:cs) = putChar c : putCharList cs

< putStr  :: String -> IO ()
< putStr s = sequence_ (putCharList s)

< openWindow :: Title -> Size -> IO Window
< type Title = String
< type Size  = (Int,Int)

> main0
>   = runGraphics (
>     do w <- openWindow 
>               "My First Graphics Program" (300,300)
>        drawInWindow w (text (100,200) "Hello Graphics World")
>        k <- getKey w
>        closeWindow w
>     )

< type Point = (Int,Int)

> spaceClose :: Window -> IO ()
> spaceClose w
>   = do k <- getKey w
>        if k==' ' || k == '\x0'
>           then closeWindow w
>           else spaceClose w


> main1
>   = runGraphics (
>     do w <- openWindow 
>               "My First Graphics Program" (300,300)
>        drawInWindow w (text (100,200) "Hello Graphics World")
>        spaceClose w
>     )

< ellipse      :: Point -> Point          -> Graphic
< shearEllipse :: Point -> Point -> Point -> Graphic
< line         :: Point -> Point          -> Graphic
< 
< polyline     :: [Point] -> Graphic
< polygon      :: [Point] -> Graphic
< polyBezier   :: [Point] -> Graphic

< withColor :: Color -> Graphic -> Graphic
< 
< data Color = Black | Blue | Green | Cyan
<            | Red | Magenta | Yellow | White

> pic1 = withColor Red 
>          (ellipse (150,150) (300,200))

> pic2 = withColor Blue
>          (polyline [(100,50),(200,50),
>                     (200,250),(100,250),(100,50)])
> 
> main2
>   = runGraphics (
>     do w <- openWindow 
>               "Some Graphics Figures" (300,300)
>        drawInWindow w pic1
>        drawInWindow w pic2
>        spaceClose w
>     )

> fillTri :: Window -> Int -> Int -> Int -> IO ()
> fillTri w x y size
>   = drawInWindow w (withColor Blue
>       (polygon [(x,y),(x+size,y),(x,y-size),(x,y)]))

> minSize :: Int
> minSize = 8
> 
> sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
> sierpinskiTri w x y size
>   = if size <= minSize
>     then fillTri w x y size
>     else let size2 = size `div` 2
>       in do sierpinskiTri w  x  y        size2
>             sierpinskiTri w  x (y-size2) size2
>             sierpinskiTri w (x+size2)  y size2

> main3 
>   = runGraphics (
>     do w <- openWindow "Sirpinski's Triangle" (400,400)
>        sierpinskiTri w 50 300 256
>        spaceClose w
>     )

> main3book
>   = runGraphics (
>     do w <- openWindow "Sirpinski's Triangle" (400,400)
>        drawInWindow w (withColor White
>          (polygon [(0,0),(399,0),(399,399),(0,399)]))
>        sierpinskiTri w 50 300 256 -- but make black for better contrast
>        spaceClose w
>     )

