{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Applicative
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Tpa = Tpa {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
   nome Text
   login Text
   senha Text
   deriving Show
   
Match json
    userId UserId
    opponentName Text
    surface Text default=''
    date UTCTime default=now()
    setPro Int
    setCon Int
    deriving Show
    
Set json
    matchId MatchId
    gamesPro Int
    gamesCon Int
    pointsServiceWon Int
    pointsServiceLost Int
    firstServiceIn Int
    acesCon Int
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

mkYesodData "Tpa" pRoutes

mkMessage "Tpa" "messages" "pt-br"

instance Yesod Tpa where
    authRoute _ = Just HomeR
    
    isAuthorized HomeR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized CadastroR _ = return Authorized
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

instance YesodPersist Tpa where
   type YesodPersistBackend Tpa = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
        
type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Tpa FormMessage where
    renderMessage _ _ = defaultFormMessage