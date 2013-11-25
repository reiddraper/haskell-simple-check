module Test.SimpleCheck.Arbitrary
    (
      Arbitrary(..)
    ) where

import Control.Monad
  (
    replicateM
  )

import Test.SimpleCheck.Gen
    (
      Gen(..)
    , RoseTree(..)
    , choose
    , sized
    , roseRoot
    , roseChildren
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

instance Arbitrary Int where
    arbitrary = sized $ \x -> choose (0 , x)

instance (Arbitrary a) => Arbitrary [a] where
    arbitrary = sized $ \x -> do
        size <- choose (0, x)
        let s = replicateM size (getGen arbitrary)
        Gen $ do
            roses <- s
            return $ shrinkList roses

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

shrinkList :: [RoseTree a] -> RoseTree [a]
shrinkList [] = RoseTree [] []
shrinkList trees =
    RoseTree (map roseRoot trees) $ map shrinkList $ removeRoses trees

removeRoses :: [RoseTree a] -> [[RoseTree a]]
removeRoses trees = map (excludeNth trees) [0..(pred $ length trees)]
                    ++ rosePermutations trees

excludeNth :: [a] -> Int -> [a]
excludeNth xs index = take index xs ++ drop (succ index) xs

rosePermutations :: [RoseTree a] -> [[RoseTree a]]
rosePermutations trees = concat [[update trees index child |
                                 child <- roseChildren rose] |
                                     (rose, index) <- zip trees [0..]]

update :: [a] -> Int -> a -> [a]
update xs index newVal = h ++ newVal:t
    where (h, _:t) = splitAt index xs
