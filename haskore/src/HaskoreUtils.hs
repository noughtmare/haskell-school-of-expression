-- This module was formerly called "HugsUtils" - but it was 
-- too messy to make it a "standard Hugs library" so we moved it
-- over here.
module HaskoreUtils(
	assert,
	unlinesS, concatS, rightS, leftS, centreS,
	right, left, centre, spaces,
	andOnError, butOnError,
        zeroOrMore, oneOrMore,
	) where

import Control.Monad
import Prelude hiding (catch)
import Control.Exception (catch)

-- ToDo: decide on appropriate fixities for these functions
infixr 2 `andOnError`, `butOnError`

assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False msg = ioError (userError msg)

unlinesS :: [ShowS] -> ShowS
unlinesS = concatS . map (. (showString "\n"))

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

rightS, leftS, centreS :: Int -> ShowS -> ShowS
rightS  n s = showString (right  n (s ""))
leftS   n s = showString (left   n (s ""))
centreS n s = showString (centre n (s ""))

right,left, centre :: Int -> String -> String
right  n s = spaces (n - length s) ++ s
left   n s = s ++ spaces (n - length s)
centre n s = spaces l ++ s ++ spaces (n'-l)
 where
  n' = n - length s
  l  = n' `div` 2

spaces :: Int -> String
spaces n = replicate (n `max` 0) ' '

-- Resource (de)allocation can interact badly with error handling code.
-- For example, even if the programmer has taken care that every
-- resource allocation is paired with an appropriate deallocation,
-- they might forget to release resources when an exception is
-- invoked.  For example, this program would fail to close
-- "outFile" if an error occured while operating on one of the "inFile"s.
-- 
--   cat :: String -> [String] -> IO ()
--   cat outfile files = do
--     outFile <- open outfile WriteMode
--     mapM_ (\file -> do
--   	    inFile <- open file ReadMode
--   	    copy inFile outFile
--   	    close inFile
--        ) 
--       files
--     close outFile
--
-- The following functions provide ways of ensuring that a piece of
-- "cleanup code" is executed even if an exception is raised.
--
--   "m `andOnError` k"  is like "m >> k" except that "k" gets executed
--     even if an exception is raised in "m".
--
--   "m `butOnError` k" is like "m" except that "k" gets executed if
--     an exception is raised in "m".
--
-- For example, the following version of "cat" guarantees to close all
-- files even if an error occurs.
--
--   cleancat :: String -> [String] -> IO ()
--   cleancat outfile files = do
--     outFile <- open outfile WriteMode
--     mapM_ (\file -> do
--   	    open file ReadMode   >>= \ inFile ->
--   	    copy inFile outFile  `andOnError`
--   	    close inFile
--        ) 
--       files
--      `andOnError`
--       close outFile

andOnError :: IO a -> IO b -> IO b
m `andOnError` k = (m `catch` \e -> k >> ioError e) >> k

-- Use this to add some cleanup code k that only gets executed
-- if an error occurs during execution of m.

butOnError :: IO a -> IO () -> IO a
m `butOnError` k = (m `catch` \e -> k >> ioError e)


zeroOrMore, oneOrMore :: MonadPlus m => m a -> m [a]
zeroOrMore m      = return [] `mplus` oneOrMore m
oneOrMore  m      = do { a <- m; as <- zeroOrMore m; return (a:as) }

