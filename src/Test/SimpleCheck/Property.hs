module Test.SimpleCheck.Property
  (
    Testable(..)
  ) where

import Test.SimpleCheck.Gen
  (
    Gen(..)
  )

import Test.SimpleCheck.Arbitrary
  (
    Arbitrary(..)
  )

data Result
    = MkResult
    {
      ok :: Bool
    } deriving (Show)


type Property = Gen Result

class Testable prop where
    property :: prop -> Property

instance Testable Bool where
    property = property . liftBool

instance Testable Result where
    property = return

instance (Arbitrary a, Testable prop) => Testable (a -> prop) where
    property = forAll arbitrary

liftBool :: Bool -> Result
liftBool bool = MkResult { ok = bool }

forAll :: (Testable prop) => Gen a -> (a -> prop) -> Property
forAll gen f = gen >>= property . f
