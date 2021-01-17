module Keyboard.Layouts where
import Pre
import Keyboard.Modifier.Map
import Keyboard.Key.Map.Set.Id


data Layout
  = Layout {first :: Word, last :: Word
           ,modifiers :: Id ModifierMap
           ,mapSet :: KeyMapSet_Id}
    deriving (Show, Read)
instance XML Layout where
  fromXML (XML "layout" as cs)
    = Layout {first = readBS $ as ! "first"
             ,last = readBS $ as ! "last"
             ,modifiers = Id $ as ! "modifiers"
             ,mapSet = readBS $ as ! "mapSet"}
  toXML l = printf "\t\t<layout first=\"%d\" last=\"%d\" modifiers=\"%s\" mapSet=\"%s\" />"
                   (first l)
                   (last l)
                   (unpack . unId $ modifiers l)
                   (show $ mapSet l)

newtype Layouts = Layouts {layouts :: NonEmpty Layout} deriving newtype (Show, Read)
instance XML Layouts where
  fromXML (XML "layouts" as cs) = Layouts . fromList $ filterName "layout" cs
  toXML (Layouts ls) = printf "\t<layouts>\n%s\t</layouts>" . unlines . fmap toXML $ toList ls
