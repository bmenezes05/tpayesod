{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET POST
   /erro ErroR GET
   /cadastro CadastroR GET POST
   /perfil/#UserId PerfilR GET
   /logout LogoutR GET
   /service/#UserId ServiceR GET
   /newmatch/#UserId NewmatchR GET 
   /static StaticR Static getStatic
|]