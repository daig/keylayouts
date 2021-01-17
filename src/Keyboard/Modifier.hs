{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Keyboard.Modifier where
import Pre
import Data.Char (toLower)
import Data.String (IsString(..))

newtype Modifier = Modifier {modifier_keys :: ModifierChord} deriving Show
instance XML Modifier where
  fromXML (XML "modifier" as _) = Modifier $ parseChord $ as ! "keys" where
    parseChord :: ByteString -> ModifierChord
    parseChord (words -> c) = ModifierChord $ fromList $ fmap parseModifier c where
      parseModifier :: ByteString -> (ModifierKey, ModifierStatus)
      parseModifier = \case
        (stripPrefix "command" -> Just (status -> s)) -> (Command, s)
        (stripPrefix "caps" -> Just (status -> s)) -> (Caps, s)
        (stripPrefix "any" -> Just (modifier -> (m,s)) ) -> (Mod m Any, s)
        (stripPrefix "right" -> Just (modifier -> (m,s)) ) -> (Mod m R, s)
        (modifier -> (m , s)) -> (Mod m L, s)
        _ -> error "parseModifier"
      status = \case {"" -> (:!); "?" -> (:?); _ -> error "status"}
      modifier (map toLower -> m) = case m of
        (stripPrefix "shift" -> Just (status -> s)) -> (Shift , s)
        (stripPrefix "option" -> Just (status -> s)) -> (Option , s)
        (stripPrefix "control" -> Just (status -> s)) -> (Option , s)
        x -> error $ "modifier " ++ show x
  toXML (Modifier ks) = printf "\t\t\t<modifier keys=\"%s\" />" $ printChord ks where
    printChord :: ModifierChord -> String
    printChord (ModifierChord (toList -> m)) = unwords $ fmap (\(mk,s) -> printKey mk ++ printStatus s)  m
    printKey = \case
      Caps -> "caps"
      Command -> "command"
      Mod (show -> md) s -> case s of {L -> uncap md; R -> "right" ++ md; Any -> "any" ++ md}
    uncap (a:as) = toLower a : as
    printStatus = \case {(:!) -> ""; (:?) -> "?"}
      
newtype ModifierChord = ModifierChord (Map ModifierKey ModifierStatus) deriving Show
data Mod = Shift | Option | Control deriving (Eq, Ord,Show)
data Side = L | R | Any deriving (Eq,Ord,Show)
instance Semigroup Side where L <> L = L; R <> R = R; _ <> _ = Any
data ModifierKey = Mod Mod Side | Command | Caps deriving (Show,Eq,Ord)
data ModifierStatus = (:!) {- ^ Required -} | (:?) {- ^ Irrelevant -} deriving (Show,Eq)
instance Semigroup ModifierChord where
  ModifierChord m <> n = foldrWithKey (\k v r -> include k v r) n m where
      include :: ModifierKey -> ModifierStatus -> ModifierChord -> ModifierChord
      include k v (ModifierChord m) = case k of
          Mod mod Any -> ModifierChord $ insert k v $ delete (Mod mod L) $ delete (Mod mod R) m
          Mod mod s | lookup (Mod mod $ swap s) m == Just v -> include (Mod mod Any) v (ModifierChord m)
          _ -> ModifierChord $ insert k v m
        where swap L = R; swap R = L; swap Any = Any

instance Monoid ModifierChord where mempty = ModifierChord empty

