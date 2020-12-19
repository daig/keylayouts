{-# language FlexibleInstances,LambdaCase, MagicHash, OverloadedStrings,PatternSynonyms, BlockArguments, ViewPatterns #-}
module Lib where
import Text.Printf
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Map (Map)
import Data.Word (Word8)
import Xeno.DOM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Text.Pretty.Simple
import qualified Data.Text as Text
import Data.List (find)
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

just# :: Maybe a -> a
just# = \case Just a -> a; _ -> error "just# Nothing"


newtype Id a = Id ByteString
newtype Ix a = Ix Word
newtype Group = Group Word

data Keyboard = Keyboard {keyboard_group :: Group
                         ,keyboard_id :: Word
                         ,keyboard_name :: ByteString
                         ,keyboard_maxout :: Maybe Word
                         ,keyboard_layouts :: Layouts
                         ,keyboard_modifierMap :: ModifierMap
                         ,keyboard_keyMapSets :: NonEmpty KeyMapSet
                         ,keyboard_actions :: [Action]
                         ,keyboard_terminators :: [When_Terminator]
                         }
__ = undefined

find# n x = fromXML . just# $ x & find \ a -> name a == n 
find' n x = fmap fromXML $ x & filter \ a -> name a == n 

instance FromXML Keyboard where
  fromXML (XML "keyboard" as cs)
    = Keyboard {keyboard_group = Map.findWithDefault (Group 126) "group" $ fmap (Group . readBS) as
               ,keyboard_id = Map.findWithDefault 1 "id" $ fmap (readBS) as
               ,keyboard_name = as ! "name"
               ,keyboard_maxout = (read . BS.unpack) <$> Map.lookup "maxount" as
               ,keyboard_layouts = find# "layouts" cs
               ,keyboard_modifierMap = find# "modifierMap" cs
               ,keyboard_keyMapSets = NE.fromList $ find' "keyMapSet" cs
               ,keyboard_actions = case cs & find \ a -> name a == "actions" of
                 Nothing -> []
                 Just (XML "actions" _ cs) -> fmap fromXML cs
               ,keyboard_terminators = find# "terminators" cs}

    

instance Show Keyboard where
  show (Keyboard g id n mo ls ms kms as ts) = printf "<keyboard>"

readBS :: Read a => ByteString -> a
readBS = read . BS.unpack

newtype Layouts = Layouts {layouts_layouts :: NonEmpty Layout}
instance FromXML Layouts where fromXML (XML "layouts" as cs) = Layouts . NE.fromList $ find' "layout" cs
data Layout
  = Layout {layout_first :: Word, layout_last :: Word
           ,layout_modifiers :: Id ModifierMap
           ,layout_mapSet :: KeyMapSet_Id}
instance FromXML Layout where
  fromXML (XML "layout" as cs)
    = Layout {layout_first = readBS $ as ! "first"
             ,layout_last = readBS $ as ! "last"
             ,layout_modifiers = Id $ as ! "modifiers"
             ,layout_mapSet = readBS $ as ! "mapSet"}

data ModifierMap
  = ModifierMap {modifierMap_id :: Id ModifierMap
                ,modifierMap_defaultIndex :: Ix KeyMap
                ,modifierMap_keyMapSelects :: NonEmpty KeyMapSelect}
instance FromXML ModifierMap where
  fromXML (XML "modifierMap" as cs)
    = ModifierMap {modifierMap_id = Id $ as ! "id"
                  ,modifierMap_defaultIndex = Ix . readBS $ as ! "defaultIndex"
                  ,modifierMap_keyMapSelects =  NE.fromList $ find' "keyMapSelect" cs}


data KeyMapSelect
  = KeyMapSelect {keyMapSelect_mapIndex :: Ix KeyMap
                 ,keyMapSelect_modifiers :: NonEmpty Modifier }
instance FromXML KeyMapSelect where
  fromXML (XML "keyMapSelect" as cs)
    = KeyMapSelect {keyMapSelect_mapIndex = Ix . readBS $ as ! "mapIndex"
                   ,keyMapSelect_modifiers = NE.fromList $ find' "modifier" cs
    }

newtype Modifier = Modifier {modifier_keys :: ModifierChord}
instance FromXML Modifier where
  fromXML (XML "modifier" as _) = Modifier $ parseChord $ as ! "keys"
parseChord :: ByteString -> ModifierChord
parseChord (BS.words -> c) = ModifierChord $ Map.fromList $ fmap parseModifier c
parseModifier :: ByteString -> (ModifierKey, ModifierStatus)
parseModifier = \case
  (BS.stripPrefix "command" -> Just (status -> s)) -> (Command, s)
  (BS.stripPrefix "caps" -> Just (status -> s)) -> (Caps, s)
  (BS.stripPrefix "any" -> Just (modifier -> (m,s)) ) -> (Mod m Any, s)
  (BS.stripPrefix "right" -> Just (modifier -> (m,s)) ) -> (Mod m R, s)
  (modifier -> (m , s)) -> (Mod m L, s)
  where
    status = \case {"" -> (:!); "?" -> (:?)}
    modifier = \case
      (BS.stripPrefix "shift" -> Just (status -> s)) -> (Shift , s)
      (BS.stripPrefix "option" -> Just (status -> s)) -> (Option , s)
      (BS.stripPrefix "control" -> Just (status -> s)) -> (Option , s)
newtype ModifierChord = ModifierChord (Map ModifierKey ModifierStatus)
data Mod = Shift | Option | Control deriving (Eq, Ord)
data Side = L | R | Any deriving (Eq,Ord)
swap L = R; swap R = L; swap Any = Any
instance Semigroup Side where L <> L = L; R <> R = R; _ <> _ = Any
data ModifierKey = Mod Mod Side | Command | Caps deriving (Eq,Ord)
data ModifierStatus = (:!) {- ^ Required -} | (:?) {- ^ Irrelevant -} deriving Eq
include :: ModifierKey -> ModifierStatus -> ModifierChord -> ModifierChord
include k v (ModifierChord m) = case k of
    Mod mod Any -> ModifierChord $ Map.insert k v $ Map.delete (Mod mod L) $ Map.delete (Mod mod R) m
    Mod mod s | Map.lookup (Mod mod $ swap s) m == Just v -> include (Mod mod Any) v (ModifierChord m)
    _ -> ModifierChord $ Map.insert k v m
instance Semigroup ModifierChord where
  ModifierChord m <> n = Map.foldrWithKey (\k v r -> include k v r) n m
instance Monoid ModifierChord where mempty = ModifierChord Map.empty

data KeyMapSet_Id = ANSI | JIS {- ^ Japan -} deriving (Eq, Read)
data KeyMapSet = KeyMapSet {keyMapSet_id :: KeyMapSet_Id, keyMapSet_keyMaps :: NonEmpty KeyMap}
instance FromXML KeyMapSet where
  fromXML (XML "keyMapSet" (readBS . (! "id") -> i) cs)
    = KeyMapSet {keyMapSet_id = i
                ,keyMapSet_keyMaps = NE.fromList $ find' "keyMap" cs}

data KeyMap
 = KeyMap {keyMap_index :: Ix KeyMap
          ,keyMap_base :: Maybe (KeyMapSet_Id, Ix KeyMap)
          ,keyMap_keys :: NonEmpty Key }
instance FromXML KeyMap where
  fromXML (XML "keyMap" as cs)
    = KeyMap {keyMap_index = Ix . readBS $ as ! "index"
             ,keyMap_base = (,) <$> (     readBS <$> Map.lookup "baseMapSet" as)
                                <*> (Ix . readBS <$> Map.lookup "baseIndex"  as)
             ,keyMap_keys = NE.fromList $ find' "key" cs}

data KeyCode = Virtual ByteString
data Key = KeyOutput {key_code :: KeyCode , key_output :: ByteString}
         | KeyNamedAction {key_code :: KeyCode, key_namedAction :: Id Action}
         | KeyAction {key_code :: KeyCode, key_action :: NonEmpty When}
instance FromXML Key where
  fromXML (XML "key" as cs) = let (Just (Virtual -> code), as') = Map.updateLookupWithKey (\_ _ -> Nothing) "code" as
                              in case (Map.toList as',cs) of
    ([("output",o)], []) -> KeyOutput code o
    ([("action",a)], []) -> KeyNamedAction code (Id a)
    ([], [a]) -> KeyAction code $ fromXML a
--
-- | Single Action within 'Key'
instance FromXML (NonEmpty When) where
  fromXML (XML "action" _ (fmap fromXML -> ws)) = NE.fromList ws

data Action = Action {action_id :: Id Action, action_whens :: NonEmpty When}
instance FromXML Action where
  fromXML (XML "action" ((Id . (! "id")) -> i) cs) = Action i . NE.fromList $ fmap fromXML cs

type StateNum = Word
data State = State ByteString | None
data When
  = WhenRange {when_range :: (StateNum,StateNum)
              ,when_range_next :: Maybe StateNum
              ,when_range_output :: Maybe Char
              ,when_multiplier :: Maybe Word8 -- ^ The difference between the input state and the start of the range
                                              -- is multiplied by this number, then added to the next state number
                                              -- and/or the output UTF-16 value.
              }
  | When {when_state :: State
         ,when_next :: Maybe State
         ,when_output :: Maybe ByteString
         }
instance FromXML When where
  fromXML (XML "when" as cs) = 
    let state = as ! "state"
    in case (Map.lookup "through" as, Map.lookup "next" as, Map.lookup "output" as, Map.lookup "multiplier" as) of
      (Just t,n,o,m) ->  WhenRange {when_range = (readBS state,readBS t)
                                   ,when_range_next = readBS <$> n
                                   ,when_range_output = (Text.head . Text.decodeUtf16LE) <$> o
                                   ,when_multiplier = readBS <$> m}
      (Nothing,n,o,Nothing) -> When  (parseState state) (parseState <$> n) o
      where parseState = \case "none" -> None; s -> State s

data When_Terminator = When_Terminator {when_terminator_state :: State, when_terminator_output :: ByteString}
instance FromXML [When_Terminator] where
  fromXML (XML "terminators" _ cs) = fmap fromXML cs
instance FromXML When_Terminator where
  fromXML (XML "when" as _) = When_Terminator (State $ as ! "state") (as ! "output")

pattern XML :: ByteString ->  Map ByteString ByteString ->  [Node] -> Node
pattern XML {xml_name, xml_attributes , xml_children}
  <- ((\n -> (name n
             , Map.fromList $ attributes n
             , children n)) -> (xml_name,xml_attributes,xml_children))
class FromXML a where fromXML :: Node -> a
