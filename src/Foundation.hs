{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
   nome Text
   login Text
   senha Text
   tipo Int
   deriving Show
   
PlayerCoach json
   coachId UserId
   playerId UserId
   UniquePlayerCoach coachId playerId
   
Match json
    userId UserId
    opponentName Text
    setPro Int
    setCon Int
    deriving Show
    
Set json
    matchId MatchId
    gamesPro Int
    gamesCon Int
    pointsServiceWon Int
    pointsServiceLost Int
    pointsService Int
    firstServiceIn Int
    aces Int
    forehandWon Int
    backhandWon Int
    volleyWon Int
    forceError Int
    doubleFaults Int
    forehandError Int
    backhandError Int
    volleyError Int
    deriving Show    
|]

staticFiles "static"

mkYesodData "Sitio" pRoutes

mkMessage "Sitio" "messages" "pt-br"

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage