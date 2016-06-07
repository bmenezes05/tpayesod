{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
 
module Handlers where
import Prelude
import Import
import Yesod
import Yesod.Form.Jquery
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T hiding (count)
import Text.Lucius
import Text.Julius
import Database.Persist.Postgresql
import Yesod.Form.Bootstrap3

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

formMatch :: Form Match
formMatch = renderDivs $ Match <$>
            areq hiddenField "" Nothing <*>
            areq textField "Nome do Oponente" Nothing <*>
            lift (liftIO getCurrentTime) <*>
            areq intField "Sets a favor" Nothing <*>
            areq intField "Sets contra" Nothing
            
getCadastroR :: Handler Html
getCadastroR = do
            (widget, enctype) <- generateFormPost formUser
            defaultLayout $ do
                $(whamletFile "templates/cadastro.hamlet")
                toWidget $ $(luciusFile "templates/cadastro.lucius")
                toWidget $ $(juliusFile "templates/cadastro.julius")
                addScript $ StaticR js_jquery_js
                addStylesheet $ StaticR css_bootstrap_css
                addScript $ StaticR js_bootstrap_js

postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getPerfilR :: UserId -> Handler Html
getPerfilR uid = do
        user <- runDB $ get404 uid
        
        -- user <- runDB $ (rawSql (pack $ "SELECT ?? FROM user WHERE user.user_id= " ++ (show $ unpack $ lookupSession "_ID")) []) :: Handler [(Entity User)]
                        
        matchesWon <- runDB $ (rawSql (T.pack $ "SELECT ?? FROM match \
                      WHERE match.user_id= " ++ (show $ fromSqlKey uid) ++ "\
                      AND match.set_pro > match.set_con") []) :: Handler [(Entity Match)]
                      
        matchesLost <- runDB $ (rawSql (T.pack $ "SELECT ?? FROM match \
                    WHERE match.user_id= " ++ (show $ fromSqlKey uid) ++ "\
                    AND match.set_pro > match.set_con") []) :: Handler [(Entity Match)]
                      
                      
                              
        defaultLayout $ do
            $(whamletFile "templates/perfil.hamlet")
            toWidget $ $(luciusFile "templates/perfil.lucius")
            toWidget $ $(juliusFile "templates/perfil.julius")
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_highcharts_js
            addScript $ StaticR js_bootstrap_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_sb_admin_css

getHomeR :: Handler Html
getHomeR = do
            (widget, enctype) <- generateFormPost formHome
            defaultLayout $ do
                $(whamletFile "templates/home.hamlet")
                toWidget $ $(luciusFile "templates/home.lucius")
                toWidget $ $(juliusFile "templates/home.julius")
                addScript $ StaticR js_jquery_js
                addStylesheet $ StaticR css_bootstrap_css
                addScript $ StaticR js_bootstrap_js
                

postHomeR :: Handler Html
postHomeR = do
           ((result, _), _) <- runFormPost formHome
           case result of 
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UserLogin ==. login, UserSenha ==. senha] []
                   case user of
                       Nothing -> redirect HomeR
                       Just (Entity uid u) -> setSession "_ID" (T.pack $ show $ fromSqlKey uid) >> redirect (PerfilR uid)

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
        defaultLayout $ do
            $(whamletFile "templates/service.hamlet")
            toWidget $ $(luciusFile "templates/service.lucius")
            toWidget $ $(juliusFile "templates/service.julius")     
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_highcharts_js
            addScript $ StaticR js_bootstrap_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_sb_admin_css
            
getNewmatchR :: UserId -> Handler Html
getNewmatchR uid = do
        (widget, enctype) <- generateFormPost formMatch
        defaultLayout $ do
            $(whamletFile "templates/newmatch.hamlet")
            toWidget $ $(luciusFile "templates/newmatch.lucius")
            toWidget $ $(juliusFile "templates/newmatch.julius")                 
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_bootstrap_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_sb_admin_css
            