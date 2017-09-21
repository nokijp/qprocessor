module Quantum.QProcessor
  ( Coef
  , Matrix(..)
  , Transition(..)
  , QVar(..)
  ) where

import Data.Complex

type Coef = Complex Double
data Matrix = Matrix Coef Coef Coef Coef deriving (Show, Eq)
data Transition = Transition Matrix [QVar] QVar deriving (Show, Eq)

newtype QVar = QVar Int deriving (Show, Eq)
