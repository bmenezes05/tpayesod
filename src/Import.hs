{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET POST
   /erro ErroR GET
   /cadastro CadastroR GET POST
   /perfil/#UserId PerfilR GET
   /logout LogoutR GET
   /service ServiceR GET
   /newmatch NewmatchR GET POST
   /static StaticR Static getStatic
|]