{-# LANGUAGE DeriveFoldable, GADTs #-}

module Test.SimpleCheck.Gen
    (
      RoseTree(..)
    , joinRose
    , roseRoot
    , roseChildren
    , filterRose
    , integralRoseTree
    , uniqueRose

    , Gen(..)
    , choose

    , sample
    , sample'
    ) where

import Prelude hiding (sequence)

import Data.List (nub)

import Text.Show.Functions()

import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  , newStdGen
  )

import Data.Set (empty, insert, notMember, singleton, Set)

import Data.Monoid
  (
    mappend
  , Monoid(..)
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
  , foldMap
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

roseChildren :: RoseTree a -> [RoseTree a]
roseChildren (RoseTree _root children) = children

-- | Return a new 'RoseTree' where all of the nodes
-- pass the predicate. Currently, if a node does
-- not pass the predicate, its children are removed too.
-- This probably not ideal behavior, but it's non-obvious
-- what should be done instead.
filterRose :: (a -> Bool) -> RoseTree a -> RoseTree a
filterRose f (RoseTree root children) =
    RoseTree root $ map (filterRose f) $ filter (f . roseRoot) children

------------------------------------------------------------------------------
-- Type: UniqueRose
------------------------------------------------------------------------------

data UniqueRose a where
    UniqueRose :: Ord a => a -> UniqueRose (RoseTree a)

instance Foldable UniqueRose where
    foldMap f (UniqueRose (RoseTree x [])) = f x
    --foldMap f (UniqueRose (RoseTree x children)) = uniqueRose f (f x) (singleton x) [children]
    foldMap f (UniqueRose (RoseTree x children)) = (f x) `mappend` (Prelude.foldr mappend mempty $ map (foldMap f) children)

uniqueRose :: (Ord a, Monoid m) => (a -> m) -> m -> Set a -> [[RoseTree a]] -> m
uniqueRose _f m _set [] = m
uniqueRose f m set ([]:t) = uniqueRose f m set t
uniqueRose f m set ((h:t):t2) = if notMember (roseRoot h) set
                             then uniqueRose f (m `mappend` (f (roseRoot h))) (insert (roseRoot h) set) $ (roseChildren h):t:t2
                             else uniqueRose f m set $ t:t2

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

mktree :: (Random a, Integral a) => StdGen -> (a, a) -> RoseTree a
mktree r rng = filterRose (mkfilter rng) $ integralRoseTree x
    where (x,_) = randomR rng r
          mkfilter (a, b) y = y >= a && y <= b

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
