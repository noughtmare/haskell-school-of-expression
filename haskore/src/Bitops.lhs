>module Bitops (module Bitops)
>  where

Bit operations work with numbers on the level of ones and zeros.
These functions should be called something like "pseudo-bitops."  They
do not reach into the ones and zeros, but they do duplicate the
effects using regular math.  Note that these bitops, though
convenient, are no more efficient than the high-level arithmetic that
does the same thing.  (This is different than in other languages such
as C.)

Shift bitwise to the left and right.

> bshiftl,bshiftr :: Int -> Int -> Int
> bshiftl b n = n*2^b
> bshiftr b n = truncate ((fromInteger (toInteger n))/2^b)

Function {\tt toBase n x} takes a given number x and "chops it up,"
returning its digits in base b.  Its output is in the form of a
big-endian list of ints.  divMod is used because it gives the correct
rounding for negative numbers.  Ex. toBytes 1000 -> toBase 256 1000 ->
(256*3) + 232 -> [ 3 , 232 ]

> toBase :: Int -> Int -> [Int]
> toBase _ 0 = []
> toBase b n = toBase b leftover ++ [digit]
>   where (leftover, digit) = n `divMod` b

> toBytes = toBase 256
> toHex   = toBase 16
> toOctal = toBase 8
> toBits  = toBase 2

Get only n of the least significant bytes of x.  If it takes less than
n digits to express x, then fill the extra digits with zeros.

> someBytes :: Int -> Int -> [Int]
> someBytes 0 _ = []
> someBytes n x = someBytes (n-1) leftover ++ [digit]
>   where (leftover, digit) = x `divMod` 256

The fromBase function converts a list of digits in another base into a
single base-10 number.

fromBase b [x,y,z] = x*b^2 + y*b^1 + z*b^0

> fromBase :: Int -> [Int] -> Int
> fromBase base xs = foldl (\a x -> base*a+x) 0 xs

> fromBytes = fromBase 256
> fromHex   = fromBase 16
> fromOctal = fromBase 8
> fromBits  = fromBase 2

bTrunc b n takes the b least significant bits of n.

> bTrunc :: Int -> Int -> Int
> bTrunc b n = n `mod` (2^b)

bSplitAt b n splits a number into a tuple: (before bit b, after bit b).

> bSplitAt :: Int -> Int -> (Int,Int)
> bSplitAt b n = n `divMod` (2^b)