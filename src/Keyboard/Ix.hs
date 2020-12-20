module Keyboard.Ix where
import Prelude

newtype Ix a = Ix {unIx :: Word} deriving Show
instance Num (Ix a) where fromInteger = Ix . fromInteger
