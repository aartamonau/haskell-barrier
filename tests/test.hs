module Main ( main )
       where


------------------------------------------------------------------------------
import Test.Framework ( defaultMain )


------------------------------------------------------------------------------
import Control.Concurrent.Barrier.Test ( tests )


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
