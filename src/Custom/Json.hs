{-# LANGUAGE ExtendedDefaultRules #-} -- Json Wanna this
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import Data.Time (getCurrentTime)

data Json = Json

mkYesod "Json" [parseRoutes|
/ HomeR GET
/error ErrorR GET
/not-found NotFoundR GET
|]

instance Yesod Json where
  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
    setTitle "Request page not found"
    toWidget [whamlet| <p> Not found |]
  errorHandler other = defaultErrorHandler other

getHomeR = do
  now <- liftIO getCurrentTime
  setMessage $ toHtml $ "Previously at: " ++ show now
  defaultLayout [whamlet| <p> Try refresh this page|]

getErrorR :: Handler ()
getErrorR = error "This is a mistake man"

getNotFoundR :: Handler ()
getNotFoundR = notFound

main :: IO ()
main = warp 3000 Json
