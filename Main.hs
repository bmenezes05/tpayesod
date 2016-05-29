{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Import
import Yesod
import Yesod.Static
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d4u3rlv2klv55f host=ec2-54-243-235-107.compute-1.amazonaws.com user=jaoqtaqjtlmbth password=oWl2bwSdwLNqvOyMMp7WT0UKKD port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Sitio t pool)