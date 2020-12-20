module Keyboard.Key.Map where
import Pre
import Keyboard.Ix
import Keyboard.Key
import Keyboard.Key.Map.Set.Id


data KeyMap
 = KeyMap {keyMap_index :: Ix KeyMap
          ,keyMap_base :: Maybe (KeyMapSet_Id, Ix KeyMap)
          ,keyMap_keys :: NonEmpty Key }
    deriving Show
instance XML KeyMap where
  fromXML (XML "keyMap" as cs)
    = KeyMap {keyMap_index = Ix . readBS $ as ! "index"
             ,keyMap_base = (,) <$> (     readBS <$> lookup "baseMapSet" as)
                                <*> (Ix . readBS <$> lookup "baseIndex"  as)
             ,keyMap_keys = fromList $ find' "key" cs}
  toXML km = printf "\t\t<keyMap index=\"%d\"%s>\n%s\t\t</keyMap>"
                 (unIx $ keyMap_index km)
                 (case keyMap_base km of
                   Nothing -> "" :: String
                   Just (bms,Ix bi) -> printf " baseMapSet=\"%s\" baseIndex=\"%d\"" (show bms) bi)
                 (unlines $ fmap toXML $ toList $ keyMap_keys km)
