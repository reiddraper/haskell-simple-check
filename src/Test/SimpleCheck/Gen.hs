{-# LANGUAGE DeriveFoldable #-}

module Test.SimpleCheck.Gen
    (
      RoseTree(..)
    , joinRose
    , roseRoot

    , Gen(..)
    , choose
    ) where

import Prelude hiding (sequence)

import Data.List (nub)

import Text.Show.Functions()

import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  )

import Control.Monad
  ( ap
  )

import Data.Traversable
  ( Traversable(..)
  , sequence
  )

import Data.Foldable
  ( Foldable(..)
  )

import Control.Applicative
  ( Applicative(..)
  , (<$>)
  )

------------------------------------------------------------------------------
-- Type: RoseTree
------------------------------------------------------------------------------

-- a RoseTree is an n-ary tree
data RoseTree a = RoseTree a [RoseTree a] deriving (Show, Foldable)

instance Traversable RoseTree where
  traverse f (RoseTree x xs) = RoseTree <$> f x <*> traverse (traverse f) xs

instance Functor RoseTree where
  fmap f (RoseTree x xs) = RoseTree (f x) $ map (fmap f) xs

instance Applicative RoseTree where
  pure  = return
  (<*>) = ap

instance Monad RoseTree where
  return x = RoseTree x []

  m >>= k = joinRose (fmap k m)

-- RoseTree helper functions

joinRose :: RoseTree (RoseTree a) -> RoseTree a
joinRose (RoseTree (RoseTree y ys) xs) =
  RoseTree y (map joinRose xs ++ ys)

roseRoot :: RoseTree a -> a
roseRoot (RoseTree x _children) = x

------------------------------------------------------------------------------
-- Type: Gen
------------------------------------------------------------------------------

newtype Gen a = MkGen {unGen :: StdGen -> Int -> a} deriving (Show)

instance Functor Gen where
  fmap f (MkGen h) =
    MkGen (\r n -> f (h r n))

instance Applicative Gen where
  pure  = return
  (<*>) = ap

instance Monad Gen where
  return x = MkGen (\_rnd _size -> x)

  MkGen m >>= k =
    MkGen (\r n ->
      let (r1,r2)  = split r
          MkGen m' = k (m r1 n)
       in m' r2 n
    )

------------------------------------------------------------------------------
-- Type: RoseGen
------------------------------------------------------------------------------

newtype RoseGen a = RoseGen { getRoseGen :: Gen (RoseTree a) }

instance Functor RoseGen where
    fmap k gen = RoseGen $ fmap (fmap k) (getRoseGen gen)

instance Monad RoseGen where
    return = RoseGen . return . return

    gen >>= f = RoseGen $ helper (getRoseGen gen) (getRoseGen . f)
        where helper m k = m >>= \y -> fmap joinRose $ sequence $ fmap k y

------------------------------------------------------------------------------
-- Functions: Combinators
------------------------------------------------------------------------------

-- | Generates a random element in the given inclusive range.
choose :: (Random a, Integral a) => (a,a) -> RoseGen a
choose rng = RoseGen $ MkGen (\r _ -> let (x,_) = randomR rng r in integralRoseTree x)

integralRoseTree :: Integral a => a -> RoseTree a
integralRoseTree x = RoseTree x $ map integralRoseTree $ shrinkIntegral x

shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral x =
  nub $
  [ -x
  | x < 0, -x > x
  ] ++
  takeWhile (<< x) (0:[ x - i | i <- tail (iterate (`quot` 2) x) ])
 where
   -- a << b is "morally" abs a < abs b, but taking care of overflow.
   a << b = case (a >= 0, b >= 0) of
            (True,  True)  -> a < b
            (False, False) -> a > b
            (True,  False) -> a + b < 0
            (False, True)  -> a + b > 0
