{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
 
module Handlers where
import Prelude hiding (length)
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Text.Julius
import Database.Persist.Postgresql

mkYesodDispatch "Tpa" pRoutes

textBoxField :: Text -> Field Handler Text
textBoxField label = Field
               { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
               , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
                 [whamlet|
                    <div class="form-group">
                        <input type="text" id=#{idAttr} name=#{nameAttr} placeholder=#{label} class="form-control">
                  |]
               , fieldEnctype = UrlEncoded
               }
               
               
passwordBoxField :: Text -> Field Handler Text
passwordBoxField label = Field
               { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
               , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
                 [whamlet|
                    <div class="form-group">
                        <input type="password" id=#{idAttr} name=#{nameAttr} placeholder=#{label} class="form-control">
                  |]
               , fieldEnctype = UrlEncoded
               }

formUser :: Form User
formUser extra = do
    (nomeRes, nomeView) <- mreq (textBoxField "Nome") "" Nothing
    (loginRes, loginView) <- mreq (textBoxField "Login") "" Nothing
    (senhaRes, senhaView) <- mreq (passwordBoxField "Senha") "" Nothing
    let userRes = User <$> nomeRes <*> loginRes <*> senhaRes 
    let widget = do
            [whamlet|
                #{extra}
                ^{fvInput nomeView}
                ^{fvInput loginView}
                ^{fvInput senhaView}
            |]
    return (userRes, widget)

formHome :: Form (Text,Text)
formHome extra = do
        (loginRes, loginView) <- mreq (textBoxField "Login") "" Nothing
        (senhaRes, senhaView) <- mreq (passwordBoxField "Senha") "" Nothing
        let userRes = (,) <$> loginRes <*> senhaRes
        let widget = do
            [whamlet|
                #{extra}
                ^{fvInput loginView}
                ^{fvInput senhaView}
            |]
        return (userRes, widget)
        

getCadastroR :: Handler Html
getCadastroR = do
            (widget, enctype) <- generateFormPost formUser
            defaultLayout $ do
                $(whamletFile "templates/cadastro.hamlet")
                toWidget $ $(luciusFile "templates/cadastro.lucius")
                addScript $ StaticR js_jquery_js
                addStylesheet $ StaticR css_bootstrap_css
                addScript $ StaticR js_bootstrap_js
                toWidget $ $(juliusFile "templates/cadastro.julius")

postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getPerfilR :: UserId -> Handler Html
getPerfilR uid = do
        user <- runDB $ get404 uid 
        matchesWon <- runDB $ (rawSql (pack $ "SELECT ?? FROM match \
                              WHERE match.user_id= " ++ (show $ fromSqlKey uid) ++ "\
                              AND match.set_pro > match.set_con") []) :: Handler [(Entity Match)]
                              
        matchesLost <- runDB $ (rawSql (pack $ "SELECT ?? FROM match \
                              WHERE match.user_id= " ++ (show $ fromSqlKey uid) ++ "\
                              AND match.set_pro > match.set_con") []) :: Handler [(Entity Match)]
                              
        defaultLayout $ do
            $(whamletFile "templates/perfil.hamlet")
            toWidget $ $(luciusFile "templates/perfil.lucius")
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_highcharts_js
            addScript $ StaticR js_bootstrap_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_sb_admin_css
            toWidget $ $(juliusFile "templates/perfil.julius")

getHomeR :: Handler Html
getHomeR = do
            (widget, enctype) <- generateFormPost formHome
            defaultLayout $ do
                $(whamletFile "templates/home.hamlet")
                toWidget $ $(luciusFile "templates/home.lucius")
                addScript $ StaticR js_jquery_js
                addStylesheet $ StaticR css_bootstrap_css
                addScript $ StaticR js_bootstrap_js
                toWidget $ $(juliusFile "templates/home.julius")

postHomeR :: Handler Html
postHomeR = do
           ((result, _), _) <- runFormPost formHome
           case result of 
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
     redirect HomeR
     
getServiceR :: UserId -> Handler Html
getServiceR uid = do
        user <- runDB $ get404 uid 
        matchesWon <- runDB $ (rawSql (pack $ "SELECT ?? FROM match \
                              WHERE match.user_id= " ++ (show $ fromSqlKey uid) ++ "\
                              AND match.set_pro > match.set_con") []) :: Handler [(Entity Match)]
                              
        matchesLost <- runDB $ (rawSql (pack $ "SELECT ?? FROM match \
                              WHERE match.user_id= " ++ (show $ fromSqlKey uid) ++ "\
                              AND match.set_pro > match.set_con") []) :: Handler [(Entity Match)]
                              
        defaultLayout $ do
            $(whamletFile "templates/service.hamlet")
            toWidget $ $(luciusFile "templates/service.lucius")
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_highcharts_js
            addScript $ StaticR js_bootstrap_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_sb_admin_css
            toWidget $ $(juliusFile "templates/service.julius")     