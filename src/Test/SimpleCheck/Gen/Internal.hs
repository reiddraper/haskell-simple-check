{-# LANGUAGE DeriveFoldable #-}

module Test.SimpleCheck.Gen.Internal
  (
    RoseTree(..)
  , roseRoot
  , roseChildren
  , joinRose
  , integralRoseTree
  , uniqueRose
  , shrinkIntegral
  ) where

import Data.Traversable
  ( Traversable(..)
  , sequence
  )

import Data.Monoid
  (
    mappend
  , Monoid(..)
  )

import Control.Applicative
  ( Applicative(..)
  , (<$>)
  )

import Control.Monad
  ( ap
  )

import Data.Foldable
  ( Foldable(..)
  )

import Data.List (nub)

import Data.Set (insert, notMember, singleton, Set)

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

roseChildren :: RoseTree a -> [RoseTree a]
roseChildren (RoseTree _root children) = children

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

uniqueRose :: (Ord a) => RoseTree a -> Set a
uniqueRose (RoseTree root children) =
    uniqueRose' singleton (singleton root) (singleton root) [children]

uniqueRose' :: (Ord a, Monoid m) => (a -> m) -> m -> Set a -> [[RoseTree a]] -> m
uniqueRose' _f m _set [] = m
uniqueRose' f m set ([]:t) = uniqueRose' f m set t
uniqueRose' f m set ((h:t):t2) = if notMember (roseRoot h) set
                             then uniqueRose' f (m `mappend` f (roseRoot h)) (insert (roseRoot h) set) $ roseChildren h:t:t2
                             else uniqueRose' f m set $ t:t2

