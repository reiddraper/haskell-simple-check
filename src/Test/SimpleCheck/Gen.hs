module Test.SimpleCheck.Gen
    (
      RoseTree(..)
    , joinRose
    , roseRoot
    , roseChildren
    , filterRose

    , Gen(..)
    , choose
    , sized

    , sample
    , sample'
    ) where

import Prelude hiding (sequence)

import Text.Show.Functions()

import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  , newStdGen
  )


import Control.Monad
  ( ap
  )

import Data.Traversable
  ( Traversable(..)
  , sequence
  )

import Control.Applicative
  (
    Applicative(..)
  )

import Test.SimpleCheck.Gen.Internal


-- | Return a new 'RoseTree' where all of the nodes
-- pass the predicate. Currently, if a node does
-- not pass the predicate, its children are removed too.
-- This probably not ideal behavior, but it's non-obvious
-- what should be done instead.
filterRose :: (a -> Bool) -> RoseTree a -> RoseTree a
filterRose f (RoseTree root children) =
    RoseTree root $ map (filterRose f) $ filter (f . roseRoot) children

------------------------------------------------------------------------------
-- Type: Gen
------------------------------------------------------------------------------

newtype Generator a = MkGen {_unGen :: StdGen -> Int -> a} deriving (Show)

instance Functor Generator where
  fmap f (MkGen h) =
    MkGen (\r n -> f (h r n))

instance Applicative Generator where
  pure  = return
  (<*>) = ap

instance Monad Generator where
  return x = MkGen (\_rnd _size -> x)

  MkGen m >>= k =
    MkGen (\r n ->
      let (r1,r2)  = split r
          MkGen m' = k (m r1 n)
       in m' r2 n
    )

------------------------------------------------------------------------------
-- Type: Gen
------------------------------------------------------------------------------

newtype Gen a = Gen { getGen :: Generator (RoseTree a) }

instance Functor Gen where
    fmap k gen = Gen $ fmap (fmap k) (getGen gen)

instance Monad Gen where
    return = Gen . return . return

    gen >>= f = Gen $ helper (getGen gen) (getGen . f)
        where helper m k = m >>= \y -> fmap joinRose $ sequence $ fmap k y

------------------------------------------------------------------------------
-- Functions: Combinators
------------------------------------------------------------------------------

-- | Generates some example values.
sample' :: Gen a -> IO [a]
sample' (Gen (MkGen m)) =
  do rnd0 <- newStdGen
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd
     return [roseRoot (m r n) | (r,n) <- rnds rnd0 `zip` [0,2..20] ]

-- | Generates some example values and prints them to 'stdout'.
sample :: Show a => Gen a -> IO ()
sample g =
  do cases <- sample' g
     mapM_ print cases

-- | Generates a random element in the given inclusive range.
choose :: (Random a, Integral a) => (a,a) -> Gen a
choose rng = Gen $ MkGen (\r _ -> mktree r rng)

-- | Used to construct generators that depend on the size parameter.
sized :: (Int -> Gen a) -> Gen a
sized f = Gen (MkGen (\r n -> let Gen (MkGen m) = f n in m r n))

mktree :: (Random a, Integral a) => StdGen -> (a, a) -> RoseTree a
mktree r rng = filterRose (mkfilter rng) $ integralRoseTree x
    where (x,_) = randomR rng r
          mkfilter (a, b) y = y >= a && y <= b

