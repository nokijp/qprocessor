module TestUtils
  ( toleranceEq
  , toleranceEqList
  , toleranceEqVector
  ) where

import Quantum.QProcessor

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Complex

toleranceEq :: Double -> Coef -> Coef -> Bool
toleranceEq tolerance a b = magnitude (a - b) <= tolerance

toleranceEqList :: Double -> [Coef] -> [Coef] -> Bool
toleranceEqList tolerance as bs = toleranceEqVector tolerance (V.fromList as) (V.fromList bs)

toleranceEqVector :: Double -> Vector Coef -> Vector Coef -> Bool
toleranceEqVector tolerance as bs = isSizeEqual && isContentsEqual
  where
    isSizeEqual = V.length as == V.length bs
    isContentsEqual = V.all id (V.zipWith (toleranceEq tolerance) as bs)
