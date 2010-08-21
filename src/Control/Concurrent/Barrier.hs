------------------------------------------------------------------------------
-- |
-- Module      : Control.Concurrent.Barrier
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Barrier synchronization for Haskell.
--
-- This library is designed to be imported qualified. Recommended import
-- statements are the following:
--
-- > import Control.Concurrent.Barrier ( Barrier )
-- > import qualified Control.Concurrent.Barrier as Barrier
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}


------------------------------------------------------------------------------
module Control.Concurrent.Barrier
       (
         -- * @Barrier@ type.
         Barrier

         -- * Creation
         , new

         -- * Waiting
         , wait
       ) where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>), (<*>), pure )
import Control.Monad ( when )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )


import Control.Concurrent.Lock ( Lock, with )
import qualified Control.Concurrent.Lock as Lock

import Control.Concurrent.Event ( Event )
import qualified Control.Concurrent.Event as Event

------------------------------------------------------------------------------
-- | Barrier data type.
data Barrier =
  Barrier { lock    :: Lock      -- ^ A lock to assure atomicity if needed.
          , parties :: Int       -- ^ Number of threads needed to proceed.
          , waiting :: IORef Int -- ^ Number of threads that currently wait
                                 -- on this barrier.
          , event   :: Event     -- ^ Event that is emitted when execution can
                                 -- be proceeded.
          }


------------------------------------------------------------------------------
-- | Creates new barrier.
new :: Int                      -- ^ Number of parties needed to wait on a
                                -- barrier to proceed.
    -> IO Barrier
new parties =
  Barrier <$> Lock.new <*> pure parties <*> newIORef 0 <*> Event.new


------------------------------------------------------------------------------
-- | Waits until needed amount of parties execute 'wait' on the barrier. After
-- the specified number of parties have called 'wait' on the barrier it is reset
-- to its initial state.
wait :: Barrier                 -- ^ A barrier to wait on.
     -> IO ()
wait (Barrier { .. }) = do
  emit <- with lock $ do
            waiting' <- readIORef waiting

            if waiting' + 1 == parties
              then do
                writeIORef waiting 0
                return True
              else do
                writeIORef waiting (waiting' + 1)
                return False

  if emit
    then Event.signal event
    else Event.wait event
