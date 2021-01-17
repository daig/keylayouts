module Keyboard.Action.When.Terminator where
import Pre
import Keyboard.Action.State

data When_Terminator = When_Terminator {when_terminator_state :: State, when_terminator_output :: ByteString}
  deriving (Show, Read)
instance XML When_Terminator where
  fromXML (XML "when" as _) = When_Terminator (State $ as ! "state") (as ! "output")
  toXML (When_Terminator s o) = printf "\t\t<when state=\"%s\" output=\"%s\" />"
                                       (case s of None -> "none"; State a -> unpack a)
                                       (unpack o)
