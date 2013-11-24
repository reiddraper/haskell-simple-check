module Main
  (
    shrinkProp
  , main
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (choose, sample')
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Data.Set (fromList)

import Test.SimpleCheck.Gen
    (
      choose
    , sample'
    , integralRoseTree
    , uniqueRose
    )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [chooseSample, treeShrinks]

chooseSample :: TestTree
chooseSample = QC.testProperty "choose respects bounds" $
    \(a, b) -> monadicIO $ do
        let mini = min a b :: Int
        let maxi = max a b :: Int
        bool <- run $ do
            samples <- sample' $ choose (mini, maxi)
            return $ all (\x -> x >= mini && x <= maxi) samples
        assert bool

treeShrinks :: TestTree
treeShrinks = localOption (QuickCheckMaxSize 20) $
    QC.testProperty "integral trees shrink correctly" shrinkProp

shrinkProp :: Int -> Bool
shrinkProp x = uniqueRose (integralRoseTree x) ==
    fromList (if x >= 0
                 then [x :: Int, x-1 .. 0]
                 else [x :: Int, x+1 .. 0] ++ [(-x), (-x)-1 .. 0])
