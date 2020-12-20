module Keyboard.Key where
import qualified Data.ByteString.Char8 as BS
import Pre
import KeyCode
import XML
import Keyboard.Id
import Keyboard.Action

data Key = KeyOutput {key_code :: KeyCode , key_output :: ByteString}
         | KeyNamedAction {key_code :: KeyCode, key_namedAction :: Id Action}
         | KeyAction {key_code :: KeyCode, key_action :: NonEmpty When}
  deriving Show
instance XML Key where
  fromXML (XML "key" as cs) = let (Just (Virtual . readBS -> code), as') = updateLookupWithKey (\_ _ -> Nothing) "code" as
                              in case (toList as',cs) of
    ([("output",o)], []) -> KeyOutput code o
    ([("action",a)], []) -> KeyNamedAction code (Id a)
    ([], [a]) -> KeyAction code $ fromXML a
    _ -> error "fromXML Key"
  toXML = \case
    KeyOutput (Virtual c) (BS.unpack -> o) -> printf "\t\t\t<key code=\"%d\" output=\"%s\" />" c o
    KeyNamedAction (Virtual c) (Id (BS.unpack -> a)) -> printf "\t\t\t<key code=\"%d\" action=\"%s\" />" c a
    KeyAction (Virtual c) ws -> printf "\t\t\t<key code=\"%d\">\n%s\t\t\t</key>" c $ toXML ws
