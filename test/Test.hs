import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (choose, sample')
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Test.SimpleCheck.Gen
    (
      choose
    , sample'
    )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [chooseSample]

chooseSample :: TestTree
chooseSample = QC.testProperty "choose respects bounds" $
    \(a, b) -> monadicIO $ do
        let mini = min a b :: Int
        let maxi = max a b :: Int
        bool <- run $ do
            samples <- sample' $ choose (mini, maxi)
            return $ all (\x -> x >= mini && x <= maxi) samples
        assert bool
