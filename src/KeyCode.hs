module KeyCode
  (KeyCode(Virtual,A,S,D,F,H,G,Z,X,C,V,B,Q,W,E,R,Y,T
          ,One,Two,Three,Four,Six,Five,Equal,Nine,Seven,Minus,Eight,Zero
          ,RightBracket,O,U,LeftBracket,I,P,L,J,Quote,K,Semicolon,Backslash
          ,Comma,Slash,N,M,Period,Grave
          ,KeypadDecimal,KeypadMultiply,KeypadPlus,KeypadClear,KeypadDivide
          ,KeypadEnter,KeypadMinus,KeypadEquals
          ,Keypad0,Keypad1,Keypad2,Keypad3,Keypad4,Keypad5,Keypad6,Keypad7,Keypad8,Keypad9
          ,Return, Tab, Space, Delete, Escape, Command, Shift, CapsLock, Option, Control
          ,RightCommand, RightShift, RightOption, RightControl, Function, F17, VolumeUp
          ,VolumeDown, Mute, F18, F19, F20, F5, F6, F7, F3, F8, F9, F11, F13, F16, F14
          ,F10, F12, F15, Help, Home, PageUp, ForwardDelete, F4, End, F2, PageDown, F1
          ,LeftArrow, RightArrow, DownArrow, UpArrow
          ,Section
          ,Yen, Underscore, KeypadComma, Eisu, Kana)
  ) where
import Pre hiding (stripPrefix)
import Data.List (stripPrefix)
import Data.Monoid (First(..))

newtype KeyCode = Virtual Word

-- * ANSI Keyboards only
pattern A = Virtual 0x00
pattern S = Virtual 0x01
pattern D = Virtual 0x02
pattern F = Virtual 0x03
pattern H = Virtual 0x04
pattern G = Virtual 0x05
pattern Z = Virtual 0x06
pattern X = Virtual 0x07
pattern C = Virtual 0x08
pattern V = Virtual 0x09
pattern B = Virtual 0x0B
pattern Q = Virtual 0x0C
pattern W = Virtual 0x0D
pattern E = Virtual 0x0E
pattern R = Virtual 0x0F
pattern Y = Virtual 0x10
pattern T = Virtual 0x11
pattern One = Virtual 0x12
pattern Two = Virtual 0x13
pattern Three = Virtual 0x14
pattern Four = Virtual 0x15
pattern Six = Virtual 0x16
pattern Five = Virtual 0x17
pattern Equal = Virtual 0x18
pattern Nine = Virtual 0x19
pattern Seven = Virtual 0x1A
pattern Minus = Virtual 0x1B
pattern Eight = Virtual 0x1C
pattern Zero = Virtual 0x1D
pattern RightBracket = Virtual 0x1E
pattern O = Virtual 0x1F
pattern U = Virtual 0x20
pattern LeftBracket = Virtual 0x21
pattern I = Virtual 0x22
pattern P = Virtual 0x23
pattern L = Virtual 0x25
pattern J = Virtual 0x26
pattern Quote = Virtual 0x27
pattern K = Virtual 0x28
pattern Semicolon = Virtual 0x29
pattern Backslash = Virtual 0x2A
pattern Comma = Virtual 0x2B
pattern Slash = Virtual 0x2C
pattern N = Virtual 0x2D
pattern M = Virtual 0x2E
pattern Period = Virtual 0x2F
pattern Grave = Virtual 0x32
pattern KeypadDecimal = Virtual 0x41
pattern KeypadMultiply = Virtual 0x43
pattern KeypadPlus = Virtual 0x45
pattern KeypadClear = Virtual 0x47
pattern KeypadDivide = Virtual 0x4B
pattern KeypadEnter = Virtual 0x4C
pattern KeypadMinus = Virtual 0x4E
pattern KeypadEquals = Virtual 0x51
pattern Keypad0 = Virtual 0x52
pattern Keypad1 = Virtual 0x53
pattern Keypad2 = Virtual 0x54
pattern Keypad3 = Virtual 0x55
pattern Keypad4 = Virtual 0x56
pattern Keypad5 = Virtual 0x57
pattern Keypad6 = Virtual 0x58
pattern Keypad7 = Virtual 0x59
pattern Keypad8 = Virtual 0x5B
pattern Keypad9 = Virtual 0x5C

--  * Independent of keyboard layout
pattern Return = Virtual 0x24
pattern Tab = Virtual 0x30
pattern Space = Virtual 0x31
pattern Delete = Virtual 0x33
pattern Escape = Virtual 0x35
pattern Command = Virtual 0x37
pattern Shift = Virtual 0x38
pattern CapsLock = Virtual 0x39
pattern Option = Virtual 0x3A
pattern Control = Virtual 0x3B
pattern RightCommand = Virtual 0x36
pattern RightShift = Virtual 0x3C
pattern RightOption = Virtual 0x3D
pattern RightControl = Virtual 0x3E
pattern Function = Virtual 0x3F
pattern F17 = Virtual 0x40
pattern VolumeUp = Virtual 0x48
pattern VolumeDown = Virtual 0x49
pattern Mute = Virtual 0x4A
pattern F18 = Virtual 0x4F
pattern F19 = Virtual 0x50
pattern F20 = Virtual 0x5A
pattern F5 = Virtual 0x60
pattern F6 = Virtual 0x61
pattern F7 = Virtual 0x62
pattern F3 = Virtual 0x63
pattern F8 = Virtual 0x64
pattern F9 = Virtual 0x65
pattern F11 = Virtual 0x67
pattern F13 = Virtual 0x69
pattern F16 = Virtual 0x6A
pattern F14 = Virtual 0x6B
pattern F10 = Virtual 0x6D
pattern F12 = Virtual 0x6F
pattern F15 = Virtual 0x71
pattern Help = Virtual 0x72
pattern Home = Virtual 0x73
pattern PageUp = Virtual 0x74
pattern ForwardDelete = Virtual 0x75
pattern F4 = Virtual 0x76
pattern End = Virtual 0x77
pattern F2 = Virtual 0x78
pattern PageDown = Virtual 0x79
pattern F1 = Virtual 0x7A
pattern LeftArrow = Virtual 0x7B
pattern RightArrow = Virtual 0x7C
pattern DownArrow = Virtual 0x7D
pattern UpArrow = Virtual 0x7E

-- * ISO keyboards only
pattern Section = Virtual 0x0A

-- * JIS keyboards only
pattern Yen = Virtual 0x5D
pattern Underscore = Virtual 0x5E
pattern KeypadComma = Virtual 0x5F
pattern Eisu = Virtual 0x66
pattern Kana = Virtual 0x68

keycodes :: [KeyCode]
keycodes =
  [ One, Two, Three, Four, Six, Five, Equal, Nine, Seven, Minus
  ,Eight, Zero, RightBracket, LeftBracket, Quote
  , Semicolon, Backslash, Comma, Slash, Period, Grave, KeypadDecimal
  , KeypadMultiply, KeypadPlus, KeypadClear, KeypadDivide, KeypadEnter, KeypadMinus
  , KeypadEquals, Keypad0, Keypad1, Keypad2, Keypad3, Keypad4, Keypad5, Keypad6, Keypad7, Keypad8, Keypad9
  ,Return, Tab, Space, Delete, Escape, Command, Shift, CapsLock, Option, Control, RightCommand, RightShift
  , RightOption, RightControl, Function, F17, VolumeUp, VolumeDown, Mute, F18, F19, F20, F5, F6, F7, F3, F8
  , F9, F11, F13, F16, F14, F10, F12, F15, Help, Home, PageUp, ForwardDelete, F4, End, F2, PageDown, F1
  , LeftArrow, RightArrow, DownArrow, UpArrow
  ,Section ,Yen, Underscore, KeypadComma, Eisu, Kana
  ,A, S, D, F, H, G, Z, X, C, V, B, Q, W, E, R, Y, T
  ,O, U, I, P , L , J, K, M, N
    ]

instance Show KeyCode where
  show = \case
    A -> "A"; S -> "S"; D -> "D"; F -> "F"; H -> "H"; G -> "G";
    Z -> "Z"; X -> "X"; C -> "C"; V -> "V"; B -> "B"; Q -> "Q";
    W -> "W"; E -> "E"; R -> "R"; Y -> "Y"; T -> "T";
    One -> "One"; Two -> "Two"; Three -> "Three"; Four -> "Four";
    Six -> "Six"; Five -> "Five"; Equal -> "Equal"; Nine -> "Nine";
    Seven -> "Seven"; Minus -> "Minus"; Eight -> "Eight"; Zero -> "Zero";
    RightBracket -> "RightBracket"; O -> "O"; U -> "U"; LeftBracket -> "LeftBracket";
    I -> "I"; P -> "P"; L -> "L"; J -> "J"; Quote -> "Quote"; K -> "K";
    Semicolon -> "Semicolon"; Backslash -> "Backslash"; Comma -> "Comma";
    Slash -> "Slash"; N -> "N"; M -> "M"; Period -> "Period"; Grave -> "Grave";
    KeypadDecimal -> "KeypadDecimal"; KeypadMultiply -> "KeypadMultiply";
    KeypadPlus -> "KeypadPlus"; KeypadClear -> "KeypadClear"; KeypadDivide -> "KeypadDivide";
    KeypadEnter -> "KeypadEnter"; KeypadMinus -> "KeypadMinus"; KeypadEquals -> "KeypadEquals";
    Keypad0 -> "Keypad0"; Keypad1 -> "Keypad1"; Keypad2 -> "Keypad2";
    Keypad3 -> "Keypad3"; Keypad4 -> "Keypad4"; Keypad5 -> "Keypad5";
    Keypad6 -> "Keypad6"; Keypad7 -> "Keypad7"; Keypad8 -> "Keypad8"; Keypad9 -> "Keypad9";

    Return -> "Return"; Tab -> "Tab"; Space -> "Space"; Delete -> "Delete";
    Escape -> "Escape"; Command -> "Command"; Shift -> "Shift"; CapsLock -> "CapsLock";
    Option -> "Option"; Control -> "Control"; RightCommand -> "RightCommand"; RightShift -> "RightShift";
    RightOption -> "RightOption"; RightControl -> "RightControl"; Function -> "Function";
    F17 -> "F17"; VolumeUp -> "VolumeUp"; VolumeDown -> "VolumeDown";
    Mute -> "Mute"; F18 -> "F18"; F19 -> "F19"; F20 -> "F20"; F5 -> "F5";
    F6 -> "F6"; F7 -> "F7"; F3 -> "F3"; F8 -> "F8"; F9 -> "F9"; F11 -> "F11"; F13 -> "F13";
    F16 -> "F16"; F14 -> "F14"; F10 -> "F10"; F12 -> "F12"; F15 -> "F15"; Help -> "Help";
    Home -> "Home"; PageUp -> "PageUp"; ForwardDelete -> "ForwardDelete"; F4 -> "F4";
    End -> "End"; F2 -> "F2"; PageDown -> "PageDown"; F1 -> "F1"; LeftArrow -> "LeftArrow";
    RightArrow -> "RightArrow"; DownArrow -> "DownArrow"; UpArrow -> "UpArrow";

    Section -> "Section";

    Yen -> "Yen"; Underscore -> "Underscore"; KeypadComma -> "KeypadComma";
    Eisu -> "Eisu"; Kana -> "Kana";

instance Read KeyCode where
  readsPrec _ s = maybe [] pure . getFirst
                $ keycodes & foldMap parseKey
    where parseKey k = First $ (k,) <$> stripPrefix (show k) s
