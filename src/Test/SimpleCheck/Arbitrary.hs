module Test.SimpleCheck.Arbitrary
    (
      Arbitrary(..)
    ) where

import Test.SimpleCheck.Gen
    (
      Gen(..)
    , choose
    , sized
    )

------------------------------------------------------------------------------
-- Class: Arbitrary
------------------------------------------------------------------------------

class Arbitrary a where
  arbitrary :: Gen a

------------------------------------------------------------------------------
-- Instances: Arbitrary
------------------------------------------------------------------------------

instance Arbitrary Integer where
    arbitrary = sized $ \x -> choose (0 :: Integer, fromIntegral x)
