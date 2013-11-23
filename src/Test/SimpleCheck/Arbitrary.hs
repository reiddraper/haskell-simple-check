module Test.SimpleCheck.Arbitrary
    (
      Arbitrary(..)
    ) where

import Test.SimpleCheck.Gen
    ( RoseTree(..)
    , Gen(..)
    )

------------------------------------------------------------------------------
-- Class: Arbitrary
------------------------------------------------------------------------------

class Arbitrary a where
  arbitrary :: Gen (RoseTree a)
