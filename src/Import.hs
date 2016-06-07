{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET POST
   /erro ErroR GET
   /logout LogoutR GET
   /cadastro CadastroR GET POST
   /perfil/#UserId PerfilR GET
   /service/#UserId ServiceR GET
   /newmatch/#UserId NewmatchR GET POST
   /newset/#UserId/#MatchId NewsetR GET POST
   /tips/#UserId TipsR GET 
   /static StaticR Static getStatic
|]