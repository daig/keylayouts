{-# language MagicHash #-}
module Keyboard (module Keyboard, XenoException(..)) where
import Pre hiding (id)
import Keyboard.Layouts (Layouts)
import Keyboard.Modifier.Map (ModifierMap)
import Keyboard.Action (Action)
import Keyboard.Action.When.Terminator (When_Terminator)
import Keyboard.Key.Map.Set (KeyMapSet)

newtype Group = Group {unGroup :: Word} deriving newtype (Show, Read)

export :: Keyboard -> ByteString
export = pack . toXML

data Keyboard = Keyboard {group :: Group
                         ,id :: Word
                         ,name :: ByteString
                         ,maxout :: Maybe Word
                         ,layouts :: Layouts
                         ,modifierMap :: ModifierMap
                         ,keyMapSets :: NonEmpty KeyMapSet
                         ,actions :: [Action]
                         ,terminators :: [When_Terminator] }
  deriving (Show, Read)

instance XML Keyboard where
  fromXML (XML "keyboard" as cs)
    = Keyboard {group = findWithDefault (Group 126) "group" $ fmap (Group . readBS) as
               ,id = findWithDefault 1 "id" $ fmap (readBS) as
               ,name = as ! "name"
               ,maxout = (read . unpack) <$> lookup "maxount" as
               ,layouts = find# "layouts" cs
               ,modifierMap = find# "modifierMap" cs
               ,keyMapSets = fromList $ filterName "keyMapSet" cs
               ,actions = findName "actions" cs
               ,terminators = findName "terminators" cs}
  toXML k = printf "<keyboard group=\"%d\" id=\"%d\" name=%s %s>\n%s\n%s\n%s%s%s\n</keyboard>"
                   (group k & unGroup)
                   (id k)
                   (name k & show)
                   (maxout k & maybe @String "" (printf "maxout = \"%s\"" . show))
                   (layouts k & toXML)
                   (modifierMap k & toXML)
                   (keyMapSets k &
                     unlines . fmap toXML . toList)
                   (actions k &
                     null' @String "" (printf "\n\t<actions>\n%s\t</actions>" . toXML))
                   (terminators k &
                     null' @String "" (printf "\n\t<terminators>\n%s\t</terminators>" . toXML))
