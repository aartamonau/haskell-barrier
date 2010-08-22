------------------------------------------------------------------------------
-- |
-- Module      : Control.Concurrent.Barrier.Test
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Unit tests for 'Control.Concurrent.Barrier' module.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
module Control.Concurrent.Barrier.Test ( tests )
       where


------------------------------------------------------------------------------


------------------------------------------------------------------------------
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.MVar ( MVar, newMVar, readMVar, modifyMVar_ )
import Control.Monad ( forM_, replicateM_ )


------------------------------------------------------------------------------
import Test.Framework ( Test )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( Assertion, assertEqual )


------------------------------------------------------------------------------
import Control.Concurrent.Barrier ( Barrier )
import qualified Control.Concurrent.Barrier as Barrier


------------------------------------------------------------------------------
wait :: IO ()
wait = threadDelay 500000


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCase "wait"      test_wait,
          testCase "waitCount" test_waitCount
        ]


------------------------------------------------------------------------------
test_wait :: Assertion
test_wait = do
  b        <- Barrier.new 5
  finished <- newMVar 0

  assertFinished finished 0

  replicateM_ 4 $ do
    worker b finished
    wait
    assertFinished finished 0

  Barrier.wait b
  wait

  assertFinished finished 4

  where worker :: Barrier -> MVar Int -> IO ()
        worker b finished = do
          forkIO $ do
            Barrier.wait b
            modifyMVar_ finished (return . (+1))

          return ()

        assertFinished :: MVar Int -> Int -> IO ()
        assertFinished finished n = do
          f <- readMVar finished
          assertEqual msg n f

          where msg = "Number of finished threads is not equal to " ++ show n


------------------------------------------------------------------------------
test_waitCount :: Assertion
test_waitCount = do
  b <- Barrier.new 5

  assertWaiting b 0

  forM_ [1 .. 4] $ \i -> do
    worker b
    wait
    assertWaiting b i

  Barrier.wait b
  assertWaiting b 0

  where assertWaiting :: Barrier -> Int -> IO ()
        assertWaiting barrier n = do
          waiting <- Barrier.waitingCount barrier
          assertEqual msg n waiting

          where msg = "Number of waiting threads is not equal to " ++ show n

        worker :: Barrier -> IO ()
        worker b = do
          forkIO $ Barrier.wait b
          return ()


------------------------------------------------------------------------------
