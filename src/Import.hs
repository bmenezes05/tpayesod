{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
-- apesar do prof, por preguica, estar usando
-- uma rota / para cadastro. Nao facam em casa.
pRoutes = [parseRoutes|
   / CadastroR GET POST
   /hello HelloR GET
   /listar ListarR GET 
   /pessoa/#PessoaId PessoaR GET POST
   /depto DeptoR GET POST
|]