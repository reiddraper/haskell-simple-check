module Test.SimpleCheck.Test
  (
    quickCheck
  ) where

import System.Random
  (
    StdGen
  , newStdGen
  , split
  )

import Test.SimpleCheck.Property
  (
    Testable(..)
  , Property
  , Result(..)
  )

import Test.SimpleCheck.Gen
  (
    Gen(..)
  , Generator(..)
  , sample'
  )

import Test.SimpleCheck.Gen.Internal
  (
    RoseTree(..)
  , roseRoot
  )

quickCheck :: (Testable prop) => prop -> IO ()
quickCheck = test . property

test :: Property -> IO ()
test prop = newStdGen >>= test' 100 prop

test' :: Int -> Property -> StdGen -> IO ()
test' 0 _ _ = return ()
test' numtimes prop rnd =
    let (r0, r1)    = split rnd
        g           = unGen $ getGen prop
        rose        = g r0 5
    in if ok $ roseRoot rose
          then test' (pred numtimes) prop r1
          else shrink rose

shrink :: RoseTree Result -> IO ()
shrink = undefined
