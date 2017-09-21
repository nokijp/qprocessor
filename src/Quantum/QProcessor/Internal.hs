module Quantum.QProcessor.Internal
  ( QState(..)
  , QSingle(..)
  , amplitude
  , emptyQState
  , fromSingle
  , productQState
  , transitionState
  , measureState
  ) where

import Quantum.QProcessor

import Control.Monad.State
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.Complex

amplitude :: Coef -> Double
amplitude c = magnitude c * magnitude c

data QState = QState Int (Vector Coef) deriving (Show, Eq)
data QSingle = QSingle Coef Coef deriving (Show, Eq)

emptyQState :: QState
emptyQState = QState 0 V.empty

fromSingle :: QSingle -> QState
fromSingle (QSingle s0 s1) = QState 1 (V.fromList [s0, s1])

productQState :: QState -> QSingle -> QState
productQState (QState 0 _)  (QSingle s0 s1) = QState 1 $ V.fromList [s0, s1]
productQState (QState n ss) (QSingle s0 s1) = QState (n + 1) $ V.map (* s0) ss V.++ V.map (* s1) ss

transitionState :: Monad m => Transition -> StateT QState m ()
transitionState = modify . transition
  where
    transition :: Transition -> QState -> QState
    transition (Transition (Matrix m00 m01 m10 m11) cs (QVar t)) (QState n ss) = QState n $ V.imap transitionBit ss
      where
        transitionBit m s =
          if m .&. controllerMask == controllerMask
          then multiplyMat (m .&. targetMask == 0) s (ss V.! (m `xor` targetMask))
          else s
        controllerMask = sum ((\(QVar c) -> 1 `shift` c) <$> cs)
        targetMask = 1 `shift` t
        multiplyMat True  s0 s1 = m00 * s0 + m01 * s1
        multiplyMat False s1 s0 = m10 * s0 + m11 * s1

measureState :: Monad m => m Double -> QVar -> StateT QState m Bit
measureState rand qv@(QVar t) = do
  QState _ ss <- get

  let (indexedSs1, indexedSs0) = V.unstablePartition (targetBit . fst) (V.indexed ss)
  let p0 = prob indexedSs0
  let p1 = prob indexedSs1
  let normalizedP0 = p0 / (p0 + p1)

  r <- lift rand
  let b = boolToBit $ r >= normalizedP0
  let reductionMatrix = ifBit b (Matrix (1 / p0 :+ 0) 0 0 0) (Matrix 0 0 0 (1 / p1 :+ 0))

  transitionState $ Transition reductionMatrix [] qv
  return b
    where
      targetBit n = (n `shift` (-t)) .&. 1 == 1
      prob = sqrt . V.sum . V.map (amplitude . snd)
