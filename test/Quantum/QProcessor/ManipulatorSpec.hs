module Quantum.QProcessor.ManipulatorSpec
  ( main
  , spec
  ) where

import Quantum.QProcessor
import Quantum.QProcessor.Gate
import Quantum.QProcessor.Manipulator
import TestUtils

import Control.Monad
import Control.Monad.Identity
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "return" $ do
    it ("should return a value") $
      runManipulatorWithRandom (Identity 1) (return "value") `shouldBe` Identity "value"

  describe "newQVar" $ do
    forM_
      [ (newQVar False, [1, 0])
      , (newQVar True, [0, 1])
      , (newQVar False >> newQVar True, [0, 0, 1, 0])
      ] $ \(program, ss) ->
      it ("should create a new qubit " ++ show ss) $
        runManipulatorWithRandom (Identity 0) (program >> spyState) `shouldBe` Identity ss

  describe "measure" $ do
    forM_
      [ (0, measuringProgram, False)
      , (0.99, measuringProgram, True)
      ] $ \(rand, program, output) ->
      it ("can measure a qubit with a random value " ++ show rand) $
        runManipulatorWithRandom (Identity rand) program `shouldBe` Identity output

  describe "spyProbs" $ do
    forM_
      [ (newQVar False >> spyProbs, [1, 0])
      , (newQVar False >>= transition . hadamard >> spyProbs, [1 / 2, 1 / 2])
      ] $ \(program, probs) ->
      it ("can spy probabilities " ++ show probs) $
        runIdentity (runManipulatorWithRandom (Identity 0) program) `shouldSatisfy` toleranceEqList 1e-5 probs

  describe "runManipulator" $ do
    forM_
      [ (0, complexProgram, complexProgramOutput False)
      , (0.99, complexProgram, complexProgramOutput True)
      ] $ \(rand, program, output) ->
      it ("should run properly complexProgram with a random value " ++ show rand) $
        runIdentity (runManipulatorWithRandom (Identity rand) program) `shouldSatisfy` all id . zipWith (toleranceEqList 1e-5) output

measuringProgram :: Manipulator Bool
measuringProgram = do
  q <- newQVar False
  transition $ hadamard q
  measure q

complexProgram :: Manipulator [[Coef]]
complexProgram = do
  q0 <- newQVar False
  s1 <- spyState
  transition $ pauliX q0
  s2 <- spyState
  q1 <- newQVar False
  s3 <- spyState
  transition $ hadamard q1
  s4 <- spyState
  _ <- measure q1
  s5 <- spyState
  return [s1, s2, s3, s4, s5]

complexProgramOutput :: Bool -> [[Coef]]
complexProgramOutput b =
  [ [1, 0]
  , [0, 1]
  , [0, 1, 0, 0]
  , [0, sqrt 2 / 2, 0, sqrt 2 / 2]
  , if b then [0, 0, 0, 1] else [0, 1, 0, 0]
  ]
