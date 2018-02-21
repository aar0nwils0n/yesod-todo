module Main where

import Yesod
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit (Manager, newManager)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool
    )
import Data.Time (UTCTime, getCurrentTime)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Typeable (Typeable)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson (toJSON, encode)
import Data.ByteString.Lazy.Char8
import Network.HTTP.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Todo json
        text String
        complete Bool
        order Int
    |]

data TodoApp = TodoApp
    {
        connPool :: ConnectionPool
        , httpManage :: Manager
    }

mkYesod "TodoApp" [parseRoutes|
/todo              TodoR POST GET
|]

instance Yesod TodoApp where
    approot = ApprootStatic "http://localhost:3000"

instance YesodPersist TodoApp where
    type YesodPersistBackend TodoApp = SqlBackend
    runDB f = do
        master <- getYesod
        let pool = connPool master
        runSqlPool f pool

postTodoR :: Handler String
postTodoR = do
    todo <- requireJsonBody
    id <- runDB $ insert (todo :: Todo)
    sendResponseStatus status201 ("CREATED" :: String)

getTodoR  :: Handler String 
getTodoR = do
    entries <- runDB $ selectList [] [Desc TodoOrder]
    return . unpack $ encode entries

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "crud" 10 -- create a new pool
    -- perform any necessary migration
    runSqlPersistMPool (runMigration migrateAll) pool
    manager <- newManager tlsManagerSettings -- create a new HTTP manager
    warp 3000 $ TodoApp pool manager -- start our server
