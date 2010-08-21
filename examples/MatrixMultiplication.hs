------------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Simple matrix multiplication illustrating usage of Control.Concurrent.Barrier
-- module.
-- Beware that this is not the way you want to multiply two matrixes actually.
--
------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}


------------------------------------------------------------------------------
import Control.Concurrent ( forkIO )
import Control.Monad ( mapM, mapM_, forM_ )
import Data.Array.IO ( IOUArray )
import Data.Array.MArray ( MArray (getBounds, newArray_),
                           readArray, writeArray, newListArray )

import Text.Printf ( printf )

import Control.Concurrent.Barrier ( Barrier )
import qualified Control.Concurrent.Barrier as Barrier


------------------------------------------------------------------------------
-- | Matrix is just an unboxed mutable array of doubles.
type Matrix = IOUArray (Int, Int) Double


------------------------------------------------------------------------------
-- | Multiplies two matrixes. Spawns bunch of threads. Each thread computes one
-- element of a resulting matrix.
multiply :: Matrix -> Matrix -> IO Matrix
multiply a b = do
  (_, (ah, aw)) <- getBounds a
  (_, (bh, bw)) <- getBounds b

  result  <- newArray_ ((1, 1), (ah, bw))
  barrier <- Barrier.new (ah * bw + 1)

  let worker row col = do
        rs <- mapM (readArray a) (map (row,) [1 .. aw])
        cs <- mapM (readArray b) (map (,col) [1 .. bh])

        writeArray result (row, col) (sum $ zipWith (*) rs cs)

        Barrier.wait barrier

  mapM_ forkIO $ map (uncurry worker) [(i, j) | i <- [1 .. ah], j <- [1 .. bw]]

  Barrier.wait barrier

  return result


------------------------------------------------------------------------------
-- | Builds a matrix from list of lists.
matrix :: [[Double]] -> IO Matrix
matrix a = newListArray ((1, 1), (m, n)) (concat a)
  where m = length a
        n = length $ head a
  

------------------------------------------------------------------------------
-- | Dumps matrix to stdout.
dump :: String -> Matrix -> IO ()
dump heading a = do
  (_, (m, n)) <- getBounds a

  printf "%s:\n" heading

  forM_ [1 .. m] $ \i -> do
    forM_ [1 .. n] $ \j -> do
      v <- readArray a (i, j)

      printf "%10.2f " v
    printf "\n"


------------------------------------------------------------------------------
main :: IO ()
main = do
  a <- matrix [[1, 2, 3, 4, 5],
               [6, 7, 8, 9 ,10]]
  b <- matrix [[1, 2],
               [3, 4],
               [5, 6],
               [7, 8],
               [9, 10]]

  dump "A" a
  dump "B" b

  r <- multiply a b

  dump "Result" r
