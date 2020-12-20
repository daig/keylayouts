module Keyboard.Action.When where
import Pre
import Keyboard.Action.State
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text

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
         } deriving Show
instance XML When where
  fromXML (XML "when" as cs) = 
    let state = as ! "state"
    in case (lookup "through" as, lookup "next" as, lookup "output" as, lookup "multiplier" as) of
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
                           Just a -> printf " output=\"%s\"" $ unpack a)
    where printState = \case {None -> "none"; State s -> unpack s}
