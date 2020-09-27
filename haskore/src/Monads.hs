-------------------
-- This module defines the following common monads:
-- 
--   SR     - state reader monad
--   State  - (strict) state transformer monad
--   IOS    - IO monad with state
--   Output - output monad
--   CPS    - continuation passing monad
--
-- Most of these monads can be found in Wadler's papers about monads.
----------------------------------------------------------------

module Monads(
	-- exports a Monad and Functor instance for each of these types:
        SR,     runSR,  getSR,                 
	State,  runS,   getS,   setS,   modS,
	IOS,    runIOS, getIOS, setIOS, modIOS,
	Output, runO,   outO,
	CPS,    runCPS, callcc, getcc
	) where

----------------------------------------------------------------
-- For each type defined in this module we provide:
--
-- o A Functor instance
-- o A Monad   instance
-- o A function 
--
--     run<M> :: <M> a -> T a
--
--   which executes a computation of type M. This function usually takes
--   extra parameters (eg an input state) and returns the value of the
--   computation and some output values (eg an output state)
--
-- o State monads (ie SR, State and IOS) also provide:
--
--     get<M> :: <M> s
--     set<M> :: s -> <M> ()        -- not SR
--     mod<M> :: (s -> s) -> <M> () -- not SR
--
--   which get the current state, set the state to a new value and apply
--   a function to the state, respectively.
----------------------------------------------------------------


----------------------------------------------------------------
-- The state reader monad
----------------------------------------------------------------

newtype SR s a = SR (s -> a)

runSR :: SR s a -> s -> a
getSR :: SR s s
	   
instance Functor (SR s) where fmap f xs = do x <- xs; return (f x)

instance Monad (SR s) where
  m >>= k  = SR (\s -> case (unSR m) s of a -> (unSR (k a)) s)
  m >>  k  = SR (\s -> case (unSR m) s of _ -> (unSR k) s)
  return a = SR (\s -> a)

runSR      = unSR 
getSR      = SR (\s -> s)

-- left inverse of S (not exported)
unSR :: SR s a -> (s -> a)
unSR (SR m) = m

----------------------------------------------------------------
-- A strict state monad
----------------------------------------------------------------

newtype State s a = S (s -> (a,s))

runS    :: State s a -> s -> (a,s)
modS    :: (s -> s) -> State s ()
setS    :: s -> State s ()
getS    :: State s s
	   
instance Functor (State s) where fmap f xs = do x <- xs; return (f x)

instance Monad (State s) where
  m >>= k  = S (\s -> case (unS m) s of (a,s') -> (unS (k a)) s')
  m >>  k  = S (\s -> case (unS m) s of (_,s') -> (unS k) s')
  return a = S (\s -> (a,s))

runS        = unS 
modS f      = S (\s -> ((),f s))
setS s'     = S (\s -> ((),s'))  
getS        = S (\s -> (s,s))

-- left inverse of S (not exported)
unS :: State s a -> (s -> (a,s))
unS (S m) = m

----------------------------------------------------------------
-- A standard IO + state monad
----------------------------------------------------------------

newtype IOS s a = IOS (s -> IO (a, s))

runIOS :: IOS s a -> s -> IO (a, s)
getIOS :: IOS s s
setIOS :: s -> IOS s ()
modIOS :: (s -> s) -> IOS s s

instance Functor (IOS s) where fmap f xs = do x <- xs; return (f x)

instance Monad   (IOS s) where
 m >>= k  = IOS (\s -> unIOS m s >>= \(a,s') -> unIOS (k a) s')
 m >>  k  = IOS (\s -> unIOS m s >>= \(_,s') -> unIOS k s')
 return a = IOS (\s -> return (a,s))

runIOS        = unIOS 
modIOS f      = IOS (\s -> return (s, f s))
setIOS s'     = IOS (\s -> return ((),s'))
getIOS        = IOS (\s -> return (s,s))

-- left inverse of IOS (not exported)
unIOS :: IOS s a -> (s -> IO (a, s))
unIOS (IOS m) = m

----------------------------------------------------------------
-- An "output" monad - like that in Wadler's "Essence of Functional
-- Programming".
----------------------------------------------------------------

newtype Output s a = O (a, [s] -> [s])

runO    :: Output s a -> (a, [s])
outO    :: [s] -> Output s ()

instance Functor (Output s) where fmap f xs = do x <- xs; return (f x)

instance Monad (Output s) where
  m >>= k  = O (let (a,r) = unO m; (b,s) = unO (k a) in (b, r . s))
  m >>  k  = O (let (a,r) = unO m; (b,s) = unO k     in (b, r . s))
  return a = O (a, \s -> s)

runO (O (a,s)) = (a, s [])
outO s         = O ((), (s ++))

-- left inverse of O (not exported)
unO :: Output s a -> (a, [s] -> [s])
unO (O m) = m

----------------------------------------------------------------
-- A CPS monad - parameterised over the type of the continuation
----------------------------------------------------------------

newtype CPS r a = CPS ((a -> r) -> r)

runCPS :: CPS r a -> (a -> r) -> r
callcc :: r -> CPS r a
getcc  :: ((a -> r) -> CPS r a) -> CPS r a

instance Functor (CPS s) where fmap f xs = do x <- xs; return (f x)

instance Monad (CPS r) where
  m >>= k  = CPS (\c -> unCPS m (\a -> unCPS (k a) c)) 
  m >>  k  = CPS (\c -> unCPS m (\a -> unCPS k     c))
  return a = CPS (\c -> c a) 

runCPS        = unCPS 
callcc cc     = CPS (\c -> cc)
getcc m       = CPS (\c -> unCPS (m c) c)

unCPS :: CPS r a -> ((a -> r) -> r)
unCPS (CPS m) = m