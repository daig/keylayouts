module Keyboard.Key.Map.Select where
import Pre
import Keyboard.Ix
import Keyboard.Key.Map
import Keyboard.Modifier

data KeyMapSelect
  = KeyMapSelect {mapIndex :: Ix KeyMap
                 ,modifiers :: NonEmpty Modifier }
    deriving (Show, Read)
instance XML KeyMapSelect where
  fromXML (XML "keyMapSelect" as cs)
    = KeyMapSelect {mapIndex = Ix . readBS $ as ! "mapIndex"
                   ,modifiers = fromList $ filterName "modifier" cs
    }
  toXML kms = printf "\t\t<keyMapSelect mapIndex=\"%d\">\n%s\t\t</keyMapSelect>"
                     (unIx $ mapIndex kms)
                     (unlines . fmap toXML . toList $ modifiers kms)
