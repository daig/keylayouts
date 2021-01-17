module Keyboard.Ix where
import Prelude

newtype Ix a = Ix {unIx :: Word} deriving newtype (Show, Read, Num)
