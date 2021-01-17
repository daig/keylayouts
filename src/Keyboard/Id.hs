module Keyboard.Id where
import Prelude
import Data.ByteString.Char8 (ByteString,pack)
import Data.String (IsString(..))

newtype Id a = Id {unId :: ByteString} deriving newtype (IsString, Show)
