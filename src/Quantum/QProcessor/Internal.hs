module Quantum.QProcessor.Internal
  ( Transition(..)
  , QState(..)
  , QSingle(..)
  , amplitude
  , fromSingle
  , productState
  , runTransition
  , measure
  ) where

import Quantum.QProcessor

import Control.Monad.State
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.Complex

newtype QState = QState (Vector Coef) deriving (Show, Eq)
data QSingle = QSingle Coef Coef deriving (Show, Eq)

amplitude :: Coef -> Double
amplitude c = magnitude c * magnitude c

fromSingle :: QSingle -> QState
fromSingle (QSingle s0 s1) = QState (V.fromList [s0, s1])

productState :: QState -> QSingle -> QState
productState (QState ss) (QSingle s0 s1) = QState $ V.map (* s0) ss V.++ V.map (* s1) ss

runTransition :: Monad m => Transition -> StateT QState m ()
runTransition = modify . transition
  where
    transition :: Transition -> QState -> QState
    transition (Transition (Matrix m00 m01 m10 m11) cs t) (QState ss) = QState $ V.imap transitionBit ss
      where
        transitionBit n s =
          if n .&. controllerMask == controllerMask
          then multiplyMat (n .&. targetMask == 0) s (ss V.! (n `xor` targetMask))
          else s
        controllerMask = sum ((1 `shift`) <$> cs)
        targetMask = 1 `shift` t
        multiplyMat True  s0 s1 = m00 * s0 + m01 * s1
        multiplyMat False s1 s0 = m10 * s0 + m11 * s1

measure :: Monad m => m Double -> Int -> StateT QState m Bool
measure rand t = do
    QState ss <- get

    let (indexedSs1, indexedSs0) = V.unstablePartition (targetBit . fst) (V.indexed ss)
    let p0 = prob indexedSs0
    let p1 = prob indexedSs1
    let normalizedP0 = p0 / (p0 + p1)

    r <- lift rand
    let b = r >= normalizedP0
    let reductionMatrix = if b then Matrix 0 0 0 (1 / p1 :+ 0) else Matrix (1 / p0 :+ 0) 0 0 0

    runTransition $ Transition reductionMatrix [] t
    return b
  where
    targetBit n = (n `shift` (-t)) .&. 1 == 1
    prob = sqrt . V.sum . V.map (amplitude . snd)
