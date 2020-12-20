module Keyboard.Modifier.Map where
import Pre
import Keyboard.Key.Map
import Keyboard.Key.Map.Select

data ModifierMap
  = ModifierMap {modifierMap_id :: Id ModifierMap
                ,modifierMap_defaultIndex :: Ix KeyMap
                ,modifierMap_keyMapSelects :: NonEmpty KeyMapSelect}
      deriving Show
instance XML ModifierMap where
  fromXML (XML "modifierMap" as cs)
    = ModifierMap {modifierMap_id = Id $ as ! "id"
                  ,modifierMap_defaultIndex = Ix . readBS $ as ! "defaultIndex"
                  ,modifierMap_keyMapSelects =  fromList $ find' "keyMapSelect" cs}
  toXML m = printf "\t<modifierMap id=\"%s\" defaultIndex=\"%d\">\n%s\t</modifierMap>"
                   (unpack . unId $ modifierMap_id m)
                   (unIx $ modifierMap_defaultIndex m)
                   (unlines . fmap toXML . toList $ modifierMap_keyMapSelects m)

