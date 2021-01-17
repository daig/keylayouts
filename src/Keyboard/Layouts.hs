module Keyboard.Layouts where
import Pre
import Keyboard.Modifier.Map
import Keyboard.Key.Map.Set.Id

newtype Layouts = Layouts {layouts_layouts :: NonEmpty Layout} deriving Show
instance XML Layouts where
  fromXML (XML "layouts" as cs) = Layouts . fromList $ filterName "layout" cs
  toXML (Layouts ls) = printf "\t<layouts>\n%s\t</layouts>" . unlines . fmap toXML $ toList ls
data Layout
  = Layout {layout_first :: Word, layout_last :: Word
           ,layout_modifiers :: Id ModifierMap
           ,layout_mapSet :: KeyMapSet_Id}
    deriving Show
instance XML Layout where
  fromXML (XML "layout" as cs)
    = Layout {layout_first = readBS $ as ! "first"
             ,layout_last = readBS $ as ! "last"
             ,layout_modifiers = Id $ as ! "modifiers"
             ,layout_mapSet = readBS $ as ! "mapSet"}
  toXML l = printf "\t\t<layout first=\"%d\" last=\"%d\" modifiers=\"%s\" mapSet=\"%s\" />"
                   (layout_first l)
                   (layout_last l)
                   (unpack . unId $ layout_modifiers l)
                   (show $ layout_mapSet l)

