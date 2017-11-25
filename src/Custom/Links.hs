{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod

data Links = Links

mkYesod "Links" [parseRoutes|
/ HomeR GET
/user UserR GET
/user2 User2R GET
|]

instance Yesod Links

getHomeR = defaultLayout [whamlet| <a href=@{UserR}> Go to User|]
getUserR = defaultLayout [whamlet| <a href=@{User2R}> Go to User 2|]
getUser2R = defaultLayout [whamlet| <a href=@{HomeR}> Go to Home|]

main = warp 3000 Links
