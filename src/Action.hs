module Action where

import Types

executeAction :: Action -> IO ()
executeAction NoAction = return ()
executeAction (NotifyPlayer username msg) = return ()
executeAction (Broadcast msg) = return ()
executeAction (Bunch actions) = sequence_ $ map executeAction actions
