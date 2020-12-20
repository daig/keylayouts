module Keyboard.Key.Map.Set where
import Pre
import Keyboard.Key.Map.Set.Id
import Keyboard.Key.Map

data KeyMapSet = KeyMapSet {keyMapSet_id :: KeyMapSet_Id, keyMapSet_keyMaps :: NonEmpty KeyMap}
  deriving Show
instance XML KeyMapSet where
  fromXML (XML "keyMapSet" (readBS . (! "id") -> i) cs)
    = KeyMapSet {keyMapSet_id = i
                ,keyMapSet_keyMaps = fromList $ find' "keyMap" cs}
  toXML kms = printf "\t<keyMapSet id=\"%s\">\n%s\t</keyMapSet>"
                     (show $ keyMapSet_id kms)
                     (unlines . fmap toXML . toList $ keyMapSet_keyMaps kms)

