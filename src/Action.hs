module Action where

import qualified Subscribers as Sub
import Types

executeAction :: Action -> SubscribersVar -> IO ()
executeAction NoAction _ = return ()
executeAction (Broadcast msgs) subsVar = Sub.broadcast msgs subsVar
executeAction (Bunch actions) subsVar =
  sequence_ $ map (flip executeAction subsVar) actions
executeAction (NotifyPlayer username msg) subsVar =
  Sub.notifySingle [msg] username subsVar
