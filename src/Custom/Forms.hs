{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import Data.Text
import Control.Applicative ((<$>), (<*>))

data Forms = Forms

mkYesod "Forms" [parseRoutes|
 / HomeR GET
 /user UserR GET
 /userinfo UserInfoR GET
|]

instance Yesod Forms

instance RenderMessage Forms FormMessage where  -- this guy is obrigatory to render pages
    renderMessage _ _ = defaultFormMessage


getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet| Hello World! |]


data User = User {
  name :: Text,
  age :: Int
} deriving Show

userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ User
  <$> areq textField "Name" Nothing
  <*> areq intField "Age" Nothing


getUserR :: Handler Html
getUserR = do
  (widget, enctype) <- generateFormPost userForm
  defaultLayout [whamlet|
  <p> Just a Form
  <form method=post action=@{UserR} enctype=#{enctype}>
  ^{widget}
  <button> Submit
  |]

postUserR :: Handler Html
postUserR = do
  ((result, widget), enctype) <- runFormPost userForm
  case result of
    FormSuccess user -> defaultLayout [whamlet|<p>#{show user} |]
    _ -> defaultLayout [whamlet|
      <p> Invalid Input
      <form method=post action=@{UserR} enctype=#{enctype}>
      ^{widget}
      <button> Submit
    |]

data UserInfo = UserInfo {
  address :: Text,
  city :: Text,
  complement :: Text
} deriving Show


userInfoAForm :: Maybe UserInfo -> AForm Handler UserInfo
userInfoAForm muserInfo = UserInfo
      <$> areq textField "Adress" (address <$> muserInfo)
      <*> areq textCity "City" (city <$> muserInfo)
      <*> areq textField "Complement" (complement <$> muserInfo)
  where
    errorMessage :: Text
    errorMessage = "You are not from Manaus man!"

    textCity = check validateCity textField

    validateCity c
      | c /= "Manaus" = Left errorMessage
      | otherwise = Right c

userInfoForm :: Html -> MForm Handler (FormResult UserInfo, Widget)
userInfoForm = renderTable $ userInfoAForm $ Just $ UserInfo "Forte" "Manaus" "gray"

getUserInfoR :: Handler Html
getUserInfoR = do
  (widget, enctype) <- generateFormPost userInfoForm
  defaultLayout [whamlet|
  <p> User info form
  <form method=post action=@{UserR} enctype=#{enctype}>
  ^{widget}
  <button> Submit

  |]

main :: IO ()
main = warp 3000 Forms
