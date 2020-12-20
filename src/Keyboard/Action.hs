module Keyboard.Action (module Keyboard.Action,module X) where
import Pre
import Keyboard.Id
import Keyboard.Action.When as X

data Action = Action {action_id :: Id Action, action_whens :: NonEmpty When} deriving Show
instance XML Action where
  fromXML (XML "action" ((Id . (! "id")) -> i) cs) = Action i . fromList $ fmap fromXML cs
  toXML a = printf "\t\t<action id=\"%s\">\n%s\t\t</action>"
                   (unpack . unId $ action_id a)
                   (unlines . fmap toXML . toList $ action_whens a)

-- | Single Action within 'Key'
instance XML (NonEmpty When) where
  fromXML (XML "action" _ (fmap fromXML -> ws)) = fromList ws
  toXML = printf "<XXaction>\n%s</XXaction>" . unlines . fmap toXML . toList

