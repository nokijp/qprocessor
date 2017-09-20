module Quantum.QProcessorSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Quantum.QProcessor

import Control.Monad (forM_)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dummy" $ do
    forM_
      [ ("quantum", "quantum")
      ] $ \(input, expected) ->
      it ("should return " ++ show expected ++ " when given" ++ show input) $ dummy input `shouldBe` expected