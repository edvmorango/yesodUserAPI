{-# LANGUAGE ExtendedDefaultRules #-} -- Json Wanna this
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod

data Json = Json

mkYesod "Json" [parseRoutes|
/ HomeR GET
|]

instance Yesod Json


getHomeR = return $ object ["home" .= "Hello Json"]

main = warp 3000 Json
