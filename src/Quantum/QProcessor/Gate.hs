module Quantum.QProcessor.Gate
  ( hadamard
  , pauliX
  , pauliY
  , pauliZ
  , cnot
  , toffoli
  , phase
  , control
  ) where

import Quantum.QProcessor

import Data.Complex hiding (phase)

hadamard :: QVar -> Transition
hadamard = Transition (Matrix (sqrt 2 / 2) (sqrt 2 / 2) (sqrt 2 / 2) (- sqrt 2 / 2)) []

pauliXMatrix :: Matrix
pauliXMatrix = Matrix 0 1 1 0

pauliX :: QVar -> Transition
pauliX = Transition pauliXMatrix []

pauliY :: QVar -> Transition
pauliY = Transition (Matrix 0 (0 :+ (-1)) (0 :+ 1) 0) []

pauliZ :: QVar -> Transition
pauliZ = Transition (Matrix 1 0 0 (-1)) []

cnot :: QVar -> QVar -> Transition
cnot controller = Transition pauliXMatrix [controller]

toffoli :: QVar -> QVar -> QVar -> Transition
toffoli controller1 controller2 = Transition pauliXMatrix [controller1, controller2]

phaseMatrix :: Double -> Matrix
phaseMatrix t = Matrix 1 0 0 (exp (0 :+ t))

phase :: Double -> QVar -> Transition
phase t = Transition (phaseMatrix t) []

control :: QVar -> Transition -> Transition
control c (Transition m cs t) = Transition m (c:cs) t
