module Quantum.QProcessor
  ( Coef
  , Matrix(..)
  , Transition(..)
  , QVar(..)
  , Bit(..)
  , boolToBit
  , bitToBool
  , ifBit
  ) where

import Data.Complex

type Coef = Complex Double
data Matrix = Matrix Coef Coef Coef Coef deriving (Show, Eq)
data Transition = Transition Matrix [QVar] QVar deriving (Show, Eq)

newtype QVar = QVar Int deriving (Show, Eq)

data Bit = Zero | One deriving (Eq, Enum, Bounded)
instance Show Bit where
  show Zero = "|0>"
  show One  = "|1>"

boolToBit :: Bool -> Bit
boolToBit False = Zero
boolToBit True  = One

bitToBool :: Bit -> Bool
bitToBool Zero = False
bitToBool One  = True

ifBit :: Bit -> a -> a -> a
ifBit Zero a _ = a
ifBit One  _ b = b
