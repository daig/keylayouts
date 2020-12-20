module Keyboard.Key.Map.Select where
import Pre
import Keyboard.Ix
import Keyboard.Key.Map
import Keyboard.Modifier

data KeyMapSelect
  = KeyMapSelect {keyMapSelect_mapIndex :: Ix KeyMap
                 ,keyMapSelect_modifiers :: NonEmpty Modifier }
    deriving Show
instance XML KeyMapSelect where
  fromXML (XML "keyMapSelect" as cs)
    = KeyMapSelect {keyMapSelect_mapIndex = Ix . readBS $ as ! "mapIndex"
                   ,keyMapSelect_modifiers = fromList $ find' "modifier" cs
    }
  toXML kms = printf "\t\t<keyMapSelect mapIndex=\"%d\">\n%s\t\t</keyMapSelect>"
                     (unIx $ keyMapSelect_mapIndex kms)
                     (unlines . fmap toXML . toList $ keyMapSelect_modifiers kms)
