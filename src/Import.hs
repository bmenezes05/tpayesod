{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET POST
   /erro ErroR GET
   /cadastro CadastroR GET POST
   /perfil/#UserId PerfilR GET
   /admin AdminR GET
   /logout LogoutR GET
   /static StaticR Static getStatic
|]