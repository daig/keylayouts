module Keyboard.Key.Map.Set where
import Pre
import Keyboard.Key.Map.Set.Id
import Keyboard.Key.Map

data KeyMapSet = KeyMapSet {id :: KeyMapSet_Id, keyMaps :: NonEmpty KeyMap}
  deriving (Show, Read)
instance XML KeyMapSet where
  fromXML (XML "keyMapSet" (readBS . (! "id") -> i) cs)
    = KeyMapSet {id = i
                ,keyMaps = fromList $ filterName "keyMap" cs}
  toXML kms = printf "\t<keyMapSet id=\"%s\">\n%s\t</keyMapSet>"
                     (show $ id kms)
                     (unlines . fmap toXML . toList $ keyMaps kms)

