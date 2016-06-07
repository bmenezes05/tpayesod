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
   /newmatch/#UserId NewmatchR GET POST
   /newset/#UserId/#MatchId NewsetR GET POST
   /static StaticR Static getStatic
|]