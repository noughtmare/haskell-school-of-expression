module FractalMusic where
import HaskoreExamples
import Random

type Vector = [Float]
type Matrix = [Vector]
type AT     = Vector -> Vector
type IFS    = [AT]

-- First define some general matrix operations.
-- These will facilitate moving to higher dimensions later.

vadd :: Vector -> Vector -> Vector
vadd = zipWith (+)

vvmult :: Vector -> Vector -> Float
vvmult v1 v2 = foldl (+) 0 (zipWith (*) v1 v2)

mvmult :: Matrix -> Vector -> Vector
mvmult m v = map (vvmult v) m

cvmult :: Float -> Vector -> Vector
cvmult c v = map (c*) v

---------------------------------------------------------------------

-- The following simulates the Iterated Function System for the
-- SierPinski Triangle as described in Barnsley's "Desktop Fractal 
-- Design Handbook".

-- First the affine transformations:

w1,w2,w3 :: AT
w1 v = (cvmult 0.01 ([[50,0],[0,50],[50,0]] `mvmult` v))
       `vadd` [8,8,8]
w2 v = (cvmult 0.01 ([[50,0],[0,50],[50,0]] `mvmult` v))
       `vadd` [30,16,2]
w3 v = (cvmult 0.01 ([[50,0],[0,50],[50,0]] `mvmult` v))
       `vadd` [20,40,30]

init0 :: Vector
init0 = [0,0,0]

-- Now we have an Iterated Function System:

ws :: IFS
ws = [w1,w2,w3]

-- And here is the result:

result = scanl f init0 random
         where f init r = (ws!!r) init

-- where "random" is a list of random indices in the range 0-2,
-- which simulates flipping the coin in Barnsley.
-- "randomInts" is imported from the Random.hs library.

random = map (`mod` 3) (randomInts 1 3)

--------

mkNote [a,b,c] = Rest (b/20) :+: Note (pitch (round a)) (c/20) []

sourceToHaskore :: [[Float]] -> Music
sourceToHaskore s = chord (map mkNote s)

sth n = sourceToHaskore (take n result)
