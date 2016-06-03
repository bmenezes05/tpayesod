{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Text.Julius
import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

formUser :: Form User
formUser = renderDivs $ User
           <$> areq textField "Nome: " Nothing 
           <*> areq textField "Login: " Nothing
           <*> areq passwordField "Senha: " Nothing
           <*> areq intField "Tipo: " Nothing

formHome :: Form (Text,Text)
formHome = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getCadastroR :: Handler Html
getCadastroR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout $ do 
           [whamlet|
                 <form method=post enctype=#{enctype} action=@{CadastroR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

getPerfilR :: UserId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
            $(whamletFile "templates/perfil.hamlet")
            toWidget $ $(luciusFile "templates/perfil.lucius")
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_highcharts_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_font_awesome_css
            addStylesheet $ StaticR css_sb_admin_css
            toWidget $ $(juliusFile "templates/perfil.julius")

postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]

getHomeR :: Handler Html
getHomeR = do
            (widget, enctype) <- generateFormPost formHome
            defaultLayout $ do
                $(whamletFile "templates/home.hamlet")
                toWidget $ $(luciusFile "templates/home.lucius")
                addStylesheet $ StaticR css_bootstrap_css
                addStylesheet $ StaticR css_font_awesome_css
                toWidget $ $(juliusFile "templates/home.julius")

postHomeR :: Handler Html
postHomeR = do
           ((result, _), _) <- runFormPost formHome
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UserLogin ==. login, UserSenha ==. senha] []
                   case user of
                       Nothing -> redirect HomeR
                       Just (Entity uid u) -> setSession "_ID" (pack $ show $ fromSqlKey uid) >> redirect (PerfilR uid)

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]