module Keyboard.Action.State where
import Pre

data State = State ByteString | None deriving (Show, Read)
type StateNum = Word
