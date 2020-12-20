{-# language StandaloneDeriving, FlexibleInstances, MagicHash, OverloadedStrings,PatternSynonyms, BlockArguments, ViewPatterns #-}
module Keyboard (module Keyboard, XenoException(..)) where
import Pre
import Keyboard.Layouts
import Keyboard.Modifier.Map
import Keyboard.Action
import Keyboard.Action.When.Terminator
import Keyboard.Key.Map.Set

newtype Group = Group {unGroup :: Word} deriving Show

parse :: ByteString -> Either XenoException Keyboard
parse = fmap fromXML . Pre.parse

export :: Keyboard -> ByteString
export = pack . toXML

data Keyboard = Keyboard {keyboard_group :: Group
                         ,keyboard_id :: Word
                         ,keyboard_name :: ByteString
                         ,keyboard_maxout :: Maybe Word
                         ,keyboard_layouts :: Layouts
                         ,keyboard_modifierMap :: ModifierMap
                         ,keyboard_keyMapSets :: NonEmpty KeyMapSet
                         ,keyboard_actions :: [Action]
                         ,keyboard_terminators :: [When_Terminator] }
  deriving Show

instance XML Keyboard where
  fromXML (XML "keyboard" as cs)
    = Keyboard {keyboard_group = findWithDefault (Group 126) "group" $ fmap (Group . readBS) as
               ,keyboard_id = findWithDefault 1 "id" $ fmap (readBS) as
               ,keyboard_name = as ! "name"
               ,keyboard_maxout = (read . unpack) <$> lookup "maxount" as
               ,keyboard_layouts = find# "layouts" cs
               ,keyboard_modifierMap = find# "modifierMap" cs
               ,keyboard_keyMapSets = fromList $ find' "keyMapSet" cs
               ,keyboard_actions = case cs & find \ a -> name a == "actions" of
                 Nothing -> []
                 Just (XML "actions" _ cs) -> fmap fromXML cs
               ,keyboard_terminators = case cs & find \ a -> name a == "terminators" of
                  Nothing -> []
                  Just (XML "terminators" _ cs) -> fmap fromXML cs}
  toXML k = printf "<keyboard group=\"%d\" id=\"%d\" name=%s %s>\n%s\n%s\n%s%s%s\n</keyboard>"
                   (unGroup $ keyboard_group k)
                   (keyboard_id k)
                   (show $ keyboard_name k)
                   (case keyboard_maxout k of
                     Nothing -> ("" :: String)
                     Just i -> printf "maxout=\"%s\"" (show i) :: String)
                   (toXML $ keyboard_layouts k)
                   (toXML $ keyboard_modifierMap k)
                   (unlines . fmap toXML . toList $ keyboard_keyMapSets k)
                   (case keyboard_actions k of
                     [] -> ("" :: String)
                     as -> printf "\n\t<actions>\n%s\t</actions>" (unlines . fmap toXML $ as) :: String)
                   (case keyboard_terminators k of
                     [] -> ("" :: String)
                     ts -> printf "\n\t<terminators>\n%s\t</terminators>" (unlines . fmap toXML $ ts) :: String)

