module Keyboard.Key.Map.Set.Id where
import Prelude

data KeyMapSet_Id = ANSI | JIS {- ^ Japan -} deriving (Eq, Read,Show)
