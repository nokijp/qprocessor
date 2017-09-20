module Quantum.QProcessor
  ( Coef
  , Matrix(..)
  , Transition(..)
  ) where

import Data.Complex

type Coef = Complex Double
data Matrix = Matrix Coef Coef Coef Coef deriving (Show, Eq)
data Transition = Transition Matrix [Int] Int deriving (Show, Eq)
