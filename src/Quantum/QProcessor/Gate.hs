module Quantum.QProcessor.Gate
  ( hadamard
  , cnot
  , toffoli
  ) where

import Quantum.QProcessor.Internal

hadamard :: QVar -> Transition
hadamard = Transition (Matrix (sqrt 2 / 2) (sqrt 2 / 2) (sqrt 2 / 2) (- sqrt 2 / 2)) []

notMatrix :: Matrix
notMatrix = Matrix 0 1 1 0

cnot :: QVar -> QVar -> Transition
cnot controller = Transition notMatrix [controller]

toffoli :: QVar -> QVar -> QVar -> Transition
toffoli controller1 controller2 = Transition notMatrix [controller1, controller2]
