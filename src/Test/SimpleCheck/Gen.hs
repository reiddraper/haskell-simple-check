{-# LANGUAGE DeriveFoldable #-}

module Test.SimpleCheck.Gen
    (
      RoseTree(..)
    , joinRose
    , roseRoot

    , Gen(..)
    ) where

import Prelude hiding (sequence)

import Text.Show.Functions()

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

newtype Gen a = Gen {unGen :: Int -> a} deriving (Show)

instance Functor Gen where
  fmap k (Gen h) = Gen (k . h)

instance Applicative Gen where
  pure  = return
  (<*>) = ap

instance Monad Gen where
  return x = Gen $ const x

  Gen m >>= k =
    Gen (\n ->
          let inner = m n
              Gen k' = k inner
          in  k' n
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
