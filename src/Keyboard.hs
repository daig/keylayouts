{-# language StandaloneDeriving, FlexibleInstances, MagicHash, OverloadedStrings,PatternSynonyms, BlockArguments, ViewPatterns #-}
module Keyboard (module Keyboard, XenoException(..)) where
import Text.Printf
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Char (toLower)
import XML
import Data.String
import KeyCode (KeyCode(Virtual))

newtype Id a = Id {unId :: ByteString}
instance IsString (Id a) where fromString = Id . BS.pack
newtype Ix a = Ix {unIx :: Word}
newtype Group = Group {unGroup :: Word}

parse :: ByteString -> Either XenoException Keyboard
parse = fmap fromXML . XML.parse

export :: Keyboard -> ByteString
export = BS.pack . toXML

data Keyboard = Keyboard {keyboard_group :: Group
                         ,keyboard_id :: Word
                         ,keyboard_name :: ByteString
                         ,keyboard_maxout :: Maybe Word
                         ,keyboard_layouts :: Layouts
                         ,keyboard_modifierMap :: ModifierMap
                         ,keyboard_keyMapSets :: NonEmpty KeyMapSet
                         ,keyboard_actions :: [Action]
                         ,keyboard_terminators :: [When_Terminator] }

instance XML Keyboard where
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
                   (unlines . fmap toXML . NE.toList $ keyboard_keyMapSets k)
                   (case keyboard_actions k of
                     [] -> ("" :: String)
                     as -> printf "\n\t<actions>\n%s\t</actions>" (unlines . fmap toXML $ as) :: String)
                   (case keyboard_terminators k of
                     [] -> ("" :: String)
                     ts -> printf "\n\t<terminators>\n%s\t</terminators>" (unlines . fmap toXML $ ts) :: String)

newtype Layouts = Layouts {layouts_layouts :: NonEmpty Layout}
instance XML Layouts where
  fromXML (XML "layouts" as cs) = Layouts . NE.fromList $ find' "layout" cs
  toXML (Layouts ls) = printf "\t<layouts>\n%s\t</layouts>" . unlines . fmap toXML $ NE.toList ls
data Layout
  = Layout {layout_first :: Word, layout_last :: Word
           ,layout_modifiers :: Id ModifierMap
           ,layout_mapSet :: KeyMapSet_Id}
instance XML Layout where
  fromXML (XML "layout" as cs)
    = Layout {layout_first = readBS $ as ! "first"
             ,layout_last = readBS $ as ! "last"
             ,layout_modifiers = Id $ as ! "modifiers"
             ,layout_mapSet = readBS $ as ! "mapSet"}
  toXML l = printf "\t\t<layout first=\"%d\" last=\"%d\" modifiers=\"%s\" mapSet=\"%s\" />"
                   (layout_first l)
                   (layout_last l)
                   (BS.unpack . unId $ layout_modifiers l)
                   (show $ layout_mapSet l)

data ModifierMap
  = ModifierMap {modifierMap_id :: Id ModifierMap
                ,modifierMap_defaultIndex :: Ix KeyMap
                ,modifierMap_keyMapSelects :: NonEmpty KeyMapSelect}
instance XML ModifierMap where
  fromXML (XML "modifierMap" as cs)
    = ModifierMap {modifierMap_id = Id $ as ! "id"
                  ,modifierMap_defaultIndex = Ix . readBS $ as ! "defaultIndex"
                  ,modifierMap_keyMapSelects =  NE.fromList $ find' "keyMapSelect" cs}
  toXML m = printf "\t<modifierMap id=\"%s\" defaultIndex=\"%d\">\n%s\t</modifierMap>"
                   (BS.unpack . unId $ modifierMap_id m)
                   (unIx $ modifierMap_defaultIndex m)
                   (unlines . fmap toXML . NE.toList $ modifierMap_keyMapSelects m)

data KeyMapSelect
  = KeyMapSelect {keyMapSelect_mapIndex :: Ix KeyMap
                 ,keyMapSelect_modifiers :: NonEmpty Modifier }
instance XML KeyMapSelect where
  fromXML (XML "keyMapSelect" as cs)
    = KeyMapSelect {keyMapSelect_mapIndex = Ix . readBS $ as ! "mapIndex"
                   ,keyMapSelect_modifiers = NE.fromList $ find' "modifier" cs
    }
  toXML kms = printf "\t\t<keyMapSelect mapIndex=\"%d\">\n%s\t\t</keyMapSelect>"
                     (unIx $ keyMapSelect_mapIndex kms)
                     (unlines . fmap toXML . NE.toList $ keyMapSelect_modifiers kms)

newtype Modifier = Modifier {modifier_keys :: ModifierChord}
instance XML Modifier where
  fromXML (XML "modifier" as _) = Modifier $ parseChord $ as ! "keys" where
    parseChord :: ByteString -> ModifierChord
    parseChord (BS.words -> c) = ModifierChord $ Map.fromList $ fmap parseModifier c where
      parseModifier :: ByteString -> (ModifierKey, ModifierStatus)
      parseModifier = \case
        (BS.stripPrefix "command" -> Just (status -> s)) -> (Command, s)
        (BS.stripPrefix "caps" -> Just (status -> s)) -> (Caps, s)
        (BS.stripPrefix "any" -> Just (modifier -> (m,s)) ) -> (Mod m Any, s)
        (BS.stripPrefix "right" -> Just (modifier -> (m,s)) ) -> (Mod m R, s)
        (modifier -> (m , s)) -> (Mod m L, s)
        _ -> error "parseModifier"
      status = \case {"" -> (:!); "?" -> (:?); _ -> error "status"}
      modifier (BS.map toLower -> m) = case m of
        (BS.stripPrefix "shift" -> Just (status -> s)) -> (Shift , s)
        (BS.stripPrefix "option" -> Just (status -> s)) -> (Option , s)
        (BS.stripPrefix "control" -> Just (status -> s)) -> (Option , s)
        x -> error $ "modifier " ++ show x
  toXML (Modifier ks) = printf "\t\t\t<modifier keys=\"%s\" />" $ printChord ks where
    printChord :: ModifierChord -> String
    printChord (ModifierChord (Map.toList -> m)) = unwords $ fmap (\(mk,s) -> printKey mk ++ printStatus s)  m
    printKey = \case
      Caps -> "caps"
      Command -> "command"
      Mod (show -> md) s -> case s of {L -> uncap md; R -> "right" ++ md; Any -> "any" ++ md}
    uncap (a:as) = toLower a : as
    printStatus = \case {(:!) -> ""; (:?) -> "?"}
      
newtype ModifierChord = ModifierChord (Map ModifierKey ModifierStatus)
data Mod = Shift | Option | Control deriving (Eq, Ord)
data Side = L | R | Any deriving (Eq,Ord)
instance Semigroup Side where L <> L = L; R <> R = R; _ <> _ = Any
data ModifierKey = Mod Mod Side | Command | Caps deriving (Eq,Ord)
data ModifierStatus = (:!) {- ^ Required -} | (:?) {- ^ Irrelevant -} deriving Eq
instance Semigroup ModifierChord where
  ModifierChord m <> n = Map.foldrWithKey (\k v r -> include k v r) n m where
      include :: ModifierKey -> ModifierStatus -> ModifierChord -> ModifierChord
      include k v (ModifierChord m) = case k of
          Mod mod Any -> ModifierChord $ Map.insert k v $ Map.delete (Mod mod L) $ Map.delete (Mod mod R) m
          Mod mod s | Map.lookup (Mod mod $ swap s) m == Just v -> include (Mod mod Any) v (ModifierChord m)
          _ -> ModifierChord $ Map.insert k v m
        where swap L = R; swap R = L; swap Any = Any

instance Monoid ModifierChord where mempty = ModifierChord Map.empty

data KeyMapSet_Id = ANSI | JIS {- ^ Japan -} deriving (Eq, Read)
data KeyMapSet = KeyMapSet {keyMapSet_id :: KeyMapSet_Id, keyMapSet_keyMaps :: NonEmpty KeyMap}
instance XML KeyMapSet where
  fromXML (XML "keyMapSet" (readBS . (! "id") -> i) cs)
    = KeyMapSet {keyMapSet_id = i
                ,keyMapSet_keyMaps = NE.fromList $ find' "keyMap" cs}
  toXML kms = printf "\t<keyMapSet id=\"%s\">\n%s\t</keyMapSet>"
                     (show $ keyMapSet_id kms)
                     (unlines . fmap toXML . NE.toList $ keyMapSet_keyMaps kms)

data KeyMap
 = KeyMap {keyMap_index :: Ix KeyMap
          ,keyMap_base :: Maybe (KeyMapSet_Id, Ix KeyMap)
          ,keyMap_keys :: NonEmpty Key }
instance XML KeyMap where
  fromXML (XML "keyMap" as cs)
    = KeyMap {keyMap_index = Ix . readBS $ as ! "index"
             ,keyMap_base = (,) <$> (     readBS <$> Map.lookup "baseMapSet" as)
                                <*> (Ix . readBS <$> Map.lookup "baseIndex"  as)
             ,keyMap_keys = NE.fromList $ find' "key" cs}
  toXML km = printf "\t\t<keyMap index=\"%d\"%s>\n%s\t\t</keyMap>"
                 (unIx $ keyMap_index km)
                 (case keyMap_base km of
                   Nothing -> "" :: String
                   Just (bms,Ix bi) -> printf " baseMapSet=\"%s\" baseIndex=\"%d\"" (show bms) bi)
                 (unlines $ fmap toXML $ NE.toList $ keyMap_keys km)
 

data Key = KeyOutput {key_code :: KeyCode , key_output :: ByteString}
         | KeyNamedAction {key_code :: KeyCode, key_namedAction :: Id Action}
         | KeyAction {key_code :: KeyCode, key_action :: NonEmpty When}
instance XML Key where
  fromXML (XML "key" as cs) = let (Just (Virtual . readBS -> code), as') = Map.updateLookupWithKey (\_ _ -> Nothing) "code" as
                              in case (Map.toList as',cs) of
    ([("output",o)], []) -> KeyOutput code o
    ([("action",a)], []) -> KeyNamedAction code (Id a)
    ([], [a]) -> KeyAction code $ fromXML a
    _ -> error "fromXML Key"
  toXML = \case
    KeyOutput (Virtual c) (BS.unpack -> o) -> printf "\t\t\t<key code=\"%d\" output=\"%s\" />" c o
    KeyNamedAction (Virtual c) (Id (BS.unpack -> a)) -> printf "\t\t\t<key code=\"%d\" action=\"%s\" />" c a
    KeyAction (Virtual c) ws -> printf "\t\t\t<key code=\"%d\">\n%s\t\t\t</key>" c $ toXML ws
--
-- | Single Action within 'Key'
instance XML (NonEmpty When) where
  fromXML (XML "action" _ (fmap fromXML -> ws)) = NE.fromList ws
  toXML = printf "<XXaction>\n%s</XXaction>" . unlines . fmap toXML . NE.toList

data Action = Action {action_id :: Id Action, action_whens :: NonEmpty When}
instance XML Action where
  fromXML (XML "action" ((Id . (! "id")) -> i) cs) = Action i . NE.fromList $ fmap fromXML cs
  toXML a = printf "\t\t<action id=\"%s\">\n%s\t\t</action>"
                   (BS.unpack . unId $ action_id a)
                   (unlines . fmap toXML . NE.toList $ action_whens a)

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
instance XML When where
  fromXML (XML "when" as cs) = 
    let state = as ! "state"
    in case (Map.lookup "through" as, Map.lookup "next" as, Map.lookup "output" as, Map.lookup "multiplier" as) of
      (Just t,n,o,m) ->  WhenRange {when_range = (readBS state,readBS t)
                                   ,when_range_next = readBS <$> n
                                   ,when_range_output = (Text.head . Text.decodeUtf16LE) <$> o
                                   ,when_multiplier = readBS <$> m}
      (Nothing,n,o,Nothing) -> When  (parseState state) (parseState <$> n) o
      _ -> error "fromXML When"
      where parseState = \case "none" -> None; s -> State s
  toXML = \case
    WhenRange (a,b) n o m -> printf "\t\t\t<when state=\"%d\" through=\"%d\"%s%s%s/>" a b
                                    (case n of
                                      Nothing -> "" :: String
                                      Just a -> printf " next=\"%d\"" a)
                                    (case o of
                                      Nothing -> "" :: String
                                      Just a -> printf " output=\"%s\"" [a])
                                    (case m of
                                      Nothing -> "" :: String
                                      Just a -> printf " multiplier=\"%d\"" a)
                                      
    When s n o -> printf "\t\t\t<when state=\"%s\"%s%s />"
                         (printState s)
                         (case n of
                           Nothing -> "" :: String
                           Just a -> printf " next=\"%s\"" $ printState a)
                         (case o of
                           Nothing -> "" :: String
                           Just a -> printf " output=\"%s\"" $ BS.unpack a)
    where printState = \case {None -> "none"; State s -> BS.unpack s}

data When_Terminator = When_Terminator {when_terminator_state :: State, when_terminator_output :: ByteString}
instance XML When_Terminator where
  fromXML (XML "when" as _) = When_Terminator (State $ as ! "state") (as ! "output")
  toXML (When_Terminator s o) = printf "\t\t<when state=\"%s\" output=\"%s\" />"
                                       (case s of None -> "none"; State a -> BS.unpack a)
                                       (BS.unpack o)


deriving instance Show Keyboard
deriving instance Show Group
deriving instance Show Layouts
deriving instance Show ModifierMap
deriving instance Show KeyMapSet
deriving instance Show Action
deriving instance Show When_Terminator
deriving instance Show Layout
deriving instance Show (Id a)
deriving instance Show (Ix a)
deriving instance Show KeyMapSelect
deriving instance Show KeyMapSet_Id
deriving instance Show KeyMap
deriving instance Show When
deriving instance Show State
deriving instance Show Modifier
deriving instance Show Key
deriving instance Show ModifierChord
deriving instance Show KeyCode
deriving instance Show ModifierKey
deriving instance Show ModifierStatus
deriving instance Show Mod
deriving instance Show Side
