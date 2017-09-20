module Quantum.QProcessor.InternalSpec
  ( main
  , spec
  ) where

import Quantum.QProcessor.Internal

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Vector.Unboxed as V
import Data.Complex
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "productState" $ do
    forM_
      [ (fromSingle (QSingle 2 3), QSingle 5 7, QState (V.fromList [10, 15, 14, 21]))
      , (QState (V.fromList [2, 3, 5, 7]), QSingle 11 13, QState (V.fromList [22, 33, 55, 77, 26, 39, 65, 91]))
      ] $ \(a, b, ab) ->
      it ("should return " ++ show ab ++ " when given " ++ show a ++ " and " ++ show b) $
        productState a b `shouldBe` ab

  describe "runTransition" $ do
    forM_
      [ (Transition (Matrix 2 3 5 7) [] (QVar 0), QState (V.fromList [11, 13]), QState (V.fromList [61, 146]))
      , (Transition (Matrix 2 3 5 7) [] (QVar 0), QState (V.fromList [11, 13, 17, 19]), QState (V.fromList [61, 146, 91, 218]))
      , (Transition (Matrix 2 3 5 7) [] (QVar 1), QState (V.fromList [11, 13, 17, 19]), QState (V.fromList [73, 83, 174, 198]))
      , (Transition (Matrix 2 3 5 7) [QVar 1] (QVar 0), QState (V.fromList [11, 13, 17, 19]), QState (V.fromList [11, 13, 91, 218]))
      , (Transition (Matrix 2 3 5 7) [QVar 0] (QVar 1), QState (V.fromList [11, 13, 17, 19]), QState (V.fromList [11, 83, 17, 198]))
      , (Transition (Matrix 2 3 5 7) [QVar 1] (QVar 0), QState (V.fromList [11, 13, 17, 19, 23, 29, 31, 37]), QState (V.fromList [11, 13, 91, 218, 23, 29, 173, 414]))
      , (Transition (Matrix 2 3 5 7) [QVar 1, QVar 2] (QVar 0), QState (V.fromList [11, 13, 17, 19, 23, 29, 31, 37]), QState (V.fromList [11, 13, 17, 19, 23, 29, 173, 414]))
      , (Transition (Matrix 2 3 5 7) [QVar 0, QVar 2] (QVar 1), QState (V.fromList [11, 13, 17, 19, 23, 29, 31, 37]), QState (V.fromList [11, 13, 17, 19, 23, 169, 31, 404]))
      ] $ \(transition, initial, final) ->
      it ("should return " ++ show final ++ " when given " ++ show transition ++ " and " ++ show initial) $
        execState (runTransition transition) initial `shouldBe` final

  describe "measure" $ do
    forM_
      [ (0.99, QVar 0, QState (V.fromList [1, 0]), QState (V.fromList [1, 0]), False)
      , (0.0, QVar 0, QState (V.fromList [1, 0]), QState (V.fromList [1, 0]), False)
      , (0.99, QVar 0, QState (V.fromList [0, 1]), QState (V.fromList [0, 1]), True)
      , (0.0, QVar 0, QState (V.fromList [0, 1]), QState (V.fromList [0, 1]), True)
      , (0.49, QVar 0, QState (V.fromList [sqrt 2 / 2, sqrt 2 / 2]), QState (V.fromList [1, 0]), False)
      , (0.51, QVar 0, QState (V.fromList [sqrt 2 / 2, sqrt 2 / 2]), QState (V.fromList [0, 1]), True)
      , (0.49, QVar 0, QState (V.fromList [1 / 2, 1 / 2, 1 / 2, 1 / 2]), QState (V.fromList [sqrt 2 / 2, 0, sqrt 2 / 2, 0]), False)
      , (0.51, QVar 0, QState (V.fromList [1 / 2, 1 / 2, 1 / 2, 1 / 2]), QState (V.fromList [0, sqrt 2 / 2, 0, sqrt 2 / 2]), True)
      , (0.49, QVar 1, QState (V.fromList [1 / 2, 1 / 2, 1 / 2, 1 / 2]), QState (V.fromList [sqrt 2 / 2, sqrt 2 / 2, 0, 0]), False)
      , (0.51, QVar 1, QState (V.fromList [1 / 2, 1 / 2, 1 / 2, 1 / 2]), QState (V.fromList [0, 0, sqrt 2 / 2, sqrt 2 / 2]), True)
      ] $ \(rand, target, initial, expectedFinal@(QState expectedSs), expectedBit) ->
      it ("should return " ++ show (expectedBit, expectedFinal) ++ " when measure " ++ show target ++ " on " ++ show initial) $ do
        let (actualBit, QState actualSs) = runState (measure (Identity rand) target) initial
        actualBit `shouldBe` expectedBit
        expectedSs `shouldSatisfy` \ss -> V.all id (V.zipWith (toleranceEq 1e-5) actualSs ss)

toleranceEq :: Double -> Complex Double -> Complex Double -> Bool
toleranceEq tolerance a b = magnitude (a - b) <= tolerance
