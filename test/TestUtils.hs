{-# LANGUAGE FlexibleInstances #-}

module TestUtils
  ( ToleranceEq
  , toleranceEqList
  , toleranceEqVector
  ) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Complex

class ToleranceEq a where
  toleranceEq :: Double -> a -> a -> Bool

instance ToleranceEq Double where
  toleranceEq tolerance a b = abs (a - b) <= tolerance

instance ToleranceEq (Complex Double) where
  toleranceEq tolerance a b = magnitude (a - b) <= tolerance

toleranceEqList :: (V.Unbox a, ToleranceEq a) => Double -> [a] -> [a] -> Bool
toleranceEqList tolerance as bs = toleranceEqVector tolerance (V.fromList as) (V.fromList bs)

toleranceEqVector :: (V.Unbox a, ToleranceEq a) => Double -> Vector a -> Vector a -> Bool
toleranceEqVector tolerance as bs = isSizeEqual && isContentsEqual
  where
    isSizeEqual = V.length as == V.length bs
    isContentsEqual = V.all id (V.zipWith (toleranceEq tolerance) as bs)
