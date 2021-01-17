module Keyboard.Key.Map where
import Pre
import Keyboard.Ix
import Keyboard.Key
import Keyboard.Key.Map.Set.Id


data KeyMap
 = KeyMap {index :: Ix KeyMap
          ,base :: Maybe (KeyMapSet_Id, Ix KeyMap)
          ,keys :: NonEmpty Key }
    deriving (Show, Read)
instance XML KeyMap where
  fromXML (XML "keyMap" as cs)
    = KeyMap {index = Ix . readBS $ as ! "index"
             ,base = (,) <$> (     readBS <$> lookup "baseMapSet" as)
                                <*> (Ix . readBS <$> lookup "baseIndex"  as)
             ,keys = fromList $ filterName "key" cs}
  toXML km = printf "\t\t<keyMap index=\"%d\"%s>\n%s\t\t</keyMap>"
                 (unIx $ index km)
                 (case base km of
                   Nothing -> "" :: String
                   Just (bms,Ix bi) -> printf " baseMapSet=\"%s\" baseIndex=\"%d\"" (show bms) bi)
                 (unlines $ fmap toXML $ toList $ keys km)
