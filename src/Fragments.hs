module Fragments where
{-
colemak = KeyMapSet
  { keyMapSet_id = ANSI
          , keyMapSet_keyMaps = KeyMap
              { keyMap_index = 0
              , keyMap_base = Nothing

colemak =         [KeyNamedAction A "a"
                  , KeyNamedAction S "r"
                  , KeyNamedAction D "s"
                  , KeyNamedAction F "t"
                  , KeyNamedAction H "h"
                  , KeyNamedAction G "d"
                  , KeyNamedAction Z "z"
                  , KeyNamedAction X "x"
                  , KeyNamedAction C "c"
                  , KeyNamedAction V "v"
                  , KeyNamedAction Section "Â§"
                  , KeyNamedAction B "b"
                  , KeyNamedAction Q "q"
                  , KeyNamedAction W "w"
                  , KeyNamedAction E "f"
                  , KeyNamedAction R "p"
                  , KeyNamedAction Y "j"
                  , KeyNamedAction T "g"
                  , KeyNamedAction One "1"
                  , KeyNamedAction Two "2"
                  , KeyNamedAction Three "3"
                  , KeyNamedAction Four "4"
                  , KeyNamedAction Six "6"
                  , KeyNamedAction Five "5"
                  , KeyNamedAction Equal "="
                  , KeyNamedAction Nine "9"
                  , KeyNamedAction Seven "7" 
                  , KeyNamedAction Minus "-" 
                  , KeyNamedAction Eight "8" 
                  , KeyNamedAction Zero "0" 
                  , KeyNamedAction RightBracket "]" 
                  , KeyNamedAction O "y" 
                  , KeyNamedAction U "l" 
                  , KeyNamedAction LeftBracket "[" 
                  , KeyNamedAction I "u" 
                  , KeyNamedAction P ";" 
                  , KeyNamedAction L "i" 
                  , KeyNamedAction J "n" 
                  , KeyNamedAction Quote "'" 
                  , KeyNamedAction K "e" 
                  , KeyNamedAction Semicolon "o" 
                  , KeyNamedAction BackSlash "\\" 
                  , KeyNamedAction Comma "," 
                  , KeyNamedAction Slash "/" 
                  , KeyNamedAction N "k" 
                  , KeyNamedAction M "m" 
                  , KeyNamedAction Period "." 
                  , KeyNamedAction Space " " 
                  , KeyNamedAction Grave "`" ]

colemak_caps =  [KeyNamedAction A "A"
                , KeyNamedAction S "R"
                , KeyNamedAction D "S"
                , KeyNamedAction F "T"
                , KeyNamedAction H "H"
                , KeyNamedAction G "D"
                , KeyNamedAction Z "Z"
                , KeyNamedAction X "X"
                , KeyNamedAction C "C"
                , KeyNamedAction V "V"
                , KeyNamedAction Section "Â§"
                , KeyNamedAction B "B"
                , KeyNamedAction Q "Q"
                , KeyNamedAction W "W"
                , KeyNamedAction E "F"
                , KeyNamedAction R "P"
                , KeyNamedAction Y "J"
                , KeyNamedAction T "G"
                , KeyNamedAction One "1"
                , KeyNamedAction Two "2"
                , KeyNamedAction Three "3"
                , KeyNamedAction Four "4"
                , KeyNamedAction Six "6"
                , KeyNamedAction Five "5"
                , KeyNamedAction Equal "="
                , KeyNamedAction Nine "9"
                , KeyNamedAction Seven "7" 
                , KeyNamedAction Minus "-" 
                , KeyNamedAction Eight "8" 
                , KeyNamedAction Zero "0" 
                , KeyNamedAction RightBracket "]" 
                , KeyNamedAction O "Y" 
                , KeyNamedAction U "L" 
                , KeyNamedAction LeftBracket "[" 
                , KeyNamedAction I "U" 
                , KeyNamedAction P ";" 
                , KeyNamedAction L "I" 
                , KeyNamedAction J "N" 
                , KeyNamedAction Quote "'" 
                , KeyNamedAction K "E" 
                , KeyNamedAction Semicolon "O" 
                , KeyNamedAction BackSlash "\\" 
                , KeyNamedAction Comma "," 
                , KeyNamedAction Slash "/" 
                , KeyNamedAction N "K" 
                , KeyNamedAction M "M" 
                , KeyNamedAction Period "." 
                , KeyNamedAction Space " " 
                , KeyNamedAction Grave "~" ]

querty_caps =         [ KeyNamedAction A "A"
                      , KeyNamedAction S "S" 
                      , KeyNamedAction D "D" 
                      , KeyNamedAction F "F" 
                      , KeyNamedAction H "H" 
                      , KeyNamedAction G "G" 
                      , KeyNamedAction Z "Z" 
                      , KeyNamedAction X "X" 
                      , KeyNamedAction C "C" 
                      , KeyNamedAction V "V" 
                      , KeyNamedAction Section "Â±" 
                      , KeyNamedAction B "B" 
                      , KeyNamedAction Q "Q" 
                      , KeyNamedAction W "W" 
                      , KeyNamedAction E "E" 
                      , KeyNamedAction R "R" 
                      , KeyNamedAction Y "Y" 
                      , KeyNamedAction T "T" 
                      , KeyNamedAction One "!" 
                      , KeyNamedAction Two "@" 
                      , KeyNamedAction Three "#" 
                      , KeyNamedAction Four "$" 
                      , KeyNamedAction Six "^" 
                      , KeyNamedAction Five "%" 
                      , KeyNamedAction Equal "+" 
                      , KeyNamedAction Nine "(" 
                      , KeyNamedAction Seven "amp" 
                      , KeyNamedAction Minus "_" 
                      , KeyNamedAction Eight "*" 
                      , KeyNamedAction Zero ")" 
                      , KeyNamedAction RightBracket "}" 
                      , KeyNamedAction O "O" 
                      , KeyNamedAction U "U" 
                      , KeyNamedAction LeftBracket "{" 
                      , KeyNamedAction I "I" 
                      , KeyNamedAction P "P" 
                      , KeyOutput Return "&#x000d;" 
                      , KeyNamedAction L "L" 
                      , KeyNamedAction J "J" 
                      , KeyNamedAction Quote "quot" 
                      , KeyNamedAction K "K" 
                      , KeyNamedAction Semicolon ":" 
                      , KeyNamedAction BackSlash "|" 
                      , KeyNamedAction Comma "lt" 
                      , KeyNamedAction Slash "?" 
                      , KeyNamedAction N "N" 
                      , KeyNamedAction M "M" 
                      , KeyNamedAction Period "gt" 
                      , KeyOutput Tab "&#x0009;" 
                      , KeyNamedAction Space "â\x87§ " 
                      , KeyNamedAction Grave "~" 
                      , KeyOutput Delete "&#x0008;" 
                      , KeyOutput 52 "&#x0003;" 
                      , KeyOutput Escape "&#x001b;" 
                      , KeyOutput KeypadDecimal "." 
                      , KeyOutput 66 "*" 
                      , KeyOutput KeypadMultiply "*" 
                      , KeyOutput KeypadPlus "+" 
                      , KeyOutput 70 "+" 
                      , KeyOutput KeypadClear "&#x001b;" 
                      , KeyOutput VolumeUp "=" 
                      , KeyOutput KeypadDivide "/" 
                      , KeyOutput KeypadEnter "&#x0003;" 
                      , KeyOutput 77 "/" 
                      , KeyOutput KeypadMinus "-" 
                      , KeyOutput KeypadEquals "=" 
                      , KeyOutput Keypad0 "0" 
                      , KeyOutput Keypad1 "1" 
                      , KeyOutput Keypad2 "2" 
                      , KeyOutput Keypad3 "3" 
                      , KeyOutput Keypad4 "4" 
                      , KeyOutput Keypad5 "5" 
                      , KeyOutput Keypad6 "6" 
                      , KeyOutput Keypad7 "7" 
                      , KeyOutput Keypad8 "8" 
                      , KeyOutput Keypad9 "9" 
                      ]
                  }
querty =              [ KeyNamedAction A "a"
                      , KeyNamedAction S "s" }
                      , KeyNamedAction D "d" }
                      , KeyNamedAction F "f" }
                      , KeyNamedAction H "h" }
                      , KeyNamedAction G "g" }
                      , KeyNamedAction Z "z" }
                      , KeyNamedAction X "x" }
                      , KeyNamedAction C "c" }
                      , KeyNamedAction V "v" }
                      , KeyNamedAction Section "â\x87ªÂ§" }
                      , KeyNamedAction B "b" }
                      , KeyNamedAction Q "q" }
                      , KeyNamedAction W "w" }
                      , KeyNamedAction E "e" }
                      , KeyNamedAction R "r" }
                      , KeyNamedAction Y "y" }
                      , KeyNamedAction T "t" }
                      , KeyNamedAction One "1" }
                      , KeyNamedAction Two "2" }
                      , KeyNamedAction Three "3" }
                      , KeyNamedAction Four "4" }
                      , KeyNamedAction Six "6" }
                      , KeyNamedAction Five "5" }
                      , KeyNamedAction Equal "=" }
                      , KeyNamedAction Nine "9" }
                      , KeyNamedAction Seven "7" }
                      , KeyNamedAction Minus "-" }
                      , KeyNamedAction Eight "8" }
                      , KeyNamedAction Zero "0" }
                      , KeyNamedAction RightBracket "â\x87ª]" }
                      , KeyNamedAction O "o" }
                      , KeyNamedAction U "u" }
                      , KeyNamedAction LeftBracket "[" }
                      , KeyNamedAction I "i" }
                      , KeyNamedAction P "p" }
                      , KeyNamedAction L "l" }
                      , KeyNamedAction J "j" }
                      , KeyNamedAction Quote "'" }
                      , KeyNamedAction K "k" }
                      , KeyNamedAction Semicolon "â\x87ª;" }
                      , KeyNamedAction BackSlash "\\" }
                      , KeyNamedAction Comma "â\x87ª," }
                      , KeyNamedAction Slash "/" }
                      , KeyNamedAction N "n" }
                      , KeyNamedAction M "m" }
                      , KeyNamedAction Period "." }
                      , KeyNamedAction Grave "â\x87ª`" }
                      ]
                  }
control =             
                      , KeyOutput Return "&#x000d;" }
                      , KeyOutput Tab "&#x0009;" }
                      , KeyOutput Delete "&#x0008;" }
[ KeyOutput Escape "&#x001b;" }
                      , KeyNamedAction Space " " }
                      , KeyOutput 52 "&#x0003;" }

functions =           [ KeyOutput F5 "&#x0010;" 
                      , KeyOutput F6 "&#x0010;" 
                      , KeyOutput F7 "&#x0010;" 
                      , KeyOutput F3 "&#x0010;" 
                      , KeyOutput F8 "&#x0010;" 
                      , KeyOutput F9 "&#x0010;" 
                      , KeyOutput 102 "&#x0010;" 
                      , KeyOutput F11 "&#x0010;" 
                      , KeyOutput 104 "&#x0010;" 
                      , KeyOutput F13 "&#x0010;" 
                      , KeyOutput F16 "&#x0010;" 
                      , KeyOutput F14 "&#x0010;" 
                      , KeyOutput 108 "&#x0010;" 
                      , KeyOutput F10 "&#x0010;" 
                      , KeyOutput 110 "&#x0010;" 
                      , KeyOutput F12 "&#x0010;" 
                      , KeyOutput 112 "&#x0010;" 
                      , KeyOutput F15 "&#x0010;" 
                      , KeyOutput 114 "&#x0005;" 
                      , KeyOutput 115 "&#x0001;" 
                      , KeyOutput 116 "&#x000b;" 
                      , KeyOutput 117 "\x7f" 
                      , KeyOutput 118 "&#x0010;" 
                      , KeyOutput 119 "&#x0004;" 
                      , KeyOutput 120 "&#x0010;" 
                      , KeyOutput 121 "&#x000c;" 
                      , KeyOutput 122 "&#x0010;" 
                      , KeyOutput 123 "&#x001c;" 
                      , KeyOutput 124 "&#x001d;" 
                      , KeyOutput 125 "&#x001f;" 
                      , KeyOutput 126 "&#x001e;" ]
keypad =          [ KeyOutput 52 "&#x0003;" 
                  , KeyOutput KeypadDecimal "." 
                  , KeyOutput 66 "&#x001d;" 
                  , KeyOutput KeypadMultiply "*" 
                  , KeyOutput KeypadPlus "+" 
                  , KeyOutput 70 "&#x001c;" 
                  , KeyOutput KeypadClear "&#x001b;" 
                  , KeyOutput VolumeUp "&#x001f;" 
                  , KeyOutput KeypadDivide "/" 
                  , KeyOutput KeypadEnter "&#x0003;" 
                  , KeyOutput 77 "&#x001e;" 
                  , KeyOutput KeypadMinus "-" 
                  , KeyOutput KeypadEquals "=" 
                  , KeyOutput Keypad0 "0" 
                  , KeyOutput Keypad1 "1" 
                  , KeyOutput Keypad2 "2" 
                  , KeyOutput Keypad3 "3" 
                  , KeyOutput Keypad4 "4" 
                  , KeyOutput Keypad5 "5" 
                  , KeyOutput Keypad6 "6" 
                  , KeyOutput Keypad7 "7" 
                  , KeyOutput Keypad8 "8" 
                  , KeyOutput Keypad9 "9" ]
                  -}
