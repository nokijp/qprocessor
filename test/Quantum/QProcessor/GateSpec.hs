module Quantum.QProcessor.GateSpec
  ( main
  , spec
  ) where

import Quantum.QProcessor
import Quantum.QProcessor.Gate

import Control.Monad
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "control" $ do
    forM_
      [ (QVar 0, pauliX (QVar 1), cnot (QVar 0) (QVar 1))
      , (QVar 0, cnot (QVar 1) (QVar 2), toffoli (QVar 0) (QVar 1) (QVar 2))
      ] $ \(c, t, expected) ->
        it ("should return " ++ show expected ++ " when given " ++ show c ++ " and " ++ show t) $
          control c t `shouldBe` expected

