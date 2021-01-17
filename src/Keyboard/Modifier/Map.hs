module Keyboard.Modifier.Map where
import Pre
import Keyboard.Key.Map
import Keyboard.Key.Map.Select

data ModifierMap
  = ModifierMap {id :: Id ModifierMap
                ,defaultIndex :: Ix KeyMap
                ,keyMapSelects :: NonEmpty KeyMapSelect}
      deriving Show
instance XML ModifierMap where
  fromXML (XML "modifierMap" as cs)
    = ModifierMap {id = Id $ as ! "id"
                  ,defaultIndex = Ix . readBS $ as ! "defaultIndex"
                  ,keyMapSelects =  fromList $ filterName "keyMapSelect" cs}
  toXML m = printf "\t<modifierMap id=\"%s\" defaultIndex=\"%d\">\n%s\t</modifierMap>"
                   (unpack . unId $ id m)
                   (unIx $ defaultIndex m)
                   (unlines . fmap toXML . toList $ keyMapSelects m)

