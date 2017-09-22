{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Quantum.QProcessor.Manipulator
  ( Manipulator
  , newQVar
  , transition
  , measure
  , spyState
  , spyProbs
  , runManipulator
  , runManipulatorWithRandom
  ) where

import Quantum.QProcessor
import Quantum.QProcessor.Internal

import Control.Monad.Operational
import Control.Monad.State
import qualified Data.Vector.Unboxed as V
import System.Random

data ManipulatorI a where
  NewQVarInstr :: Bit -> ManipulatorI QVar
  TransitionInstr :: Transition -> ManipulatorI ()
  MeasureInstr :: QVar -> ManipulatorI Bit
  SpyStateInstr :: ManipulatorI [Coef]
  SpyProbsInstr :: ManipulatorI [Double]
newtype Manipulator a = Manipulator { raw :: Program ManipulatorI a } deriving (Functor, Applicative, Monad)

newQVar :: Bit -> Manipulator QVar
newQVar = Manipulator . singleton . NewQVarInstr

transition :: Transition -> Manipulator ()
transition = Manipulator . singleton . TransitionInstr

measure :: QVar -> Manipulator Bit
measure = Manipulator . singleton . MeasureInstr

spyState :: Manipulator [Coef]
spyState = Manipulator $ singleton SpyStateInstr

spyProbs :: Manipulator [Double]
spyProbs = Manipulator $ singleton SpyProbsInstr

runManipulator :: Manipulator a -> IO a
runManipulator = runManipulatorWithRandom randomIO

runManipulatorWithRandom :: Monad m => m Double -> Manipulator a -> m a
runManipulatorWithRandom rand manip = evalStateT (run rand manip) emptyQState
  where
    run :: Monad m => m Double -> Manipulator a -> StateT QState m a
    run r = eval r . view . raw
    eval :: Monad m => m Double -> ProgramView ManipulatorI a -> StateT QState m a
    eval r (NewQVarInstr b :>>= k) = do
      qs <- get
      let q = ifBit b (QSingle 1 0) (QSingle 0 1)
      let qs'@(QState n' _) = qs `productQState` q
      put qs'
      run r $ Manipulator $ k (QVar $ n' - 1)
    eval r (TransitionInstr t :>>= k) = transitionState t >> run r (Manipulator (k ()))
    eval r (MeasureInstr q :>>= k) = measureState r q >>= run r . Manipulator. k
    eval r (SpyStateInstr :>>= k) = do
      QState _ ss <- get
      run r $ Manipulator $ k (V.toList ss)
    eval r (SpyProbsInstr :>>= k) = do
      QState _ ss <- get
      run r $ Manipulator $ k (amplitude <$> V.toList ss)
    eval _ (Return x) = return x
