module Concurrent 
  (Chan, isEmptyChan, newChan, readChan, writeChan,
   MVar, readMVar, newMVar, modifyMVar, modifyMVar_) where

import Control.Concurrent.MVar
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.STM.TChan as C

type Chan = S.TChan

isEmptyChan = S.atomically . C.isEmptyTChan
newChan = C.newTChanIO
readChan = S.atomically . C.readTChan
writeChan c = S.atomically . C.writeTChan c



