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
import Data.ByteString.Lazy.Char8 as C (unpack)
import Network.HTTP.Types
import Data.Text as T
import Control.Monad (when)
import Network.Wai (requestHeaders)
import Data.Aeson
import GHC.Generics

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

data ErrBody = ErrBody {
    message :: Text
    , success :: Bool
    } deriving (Generic, Show)
    
instance ToJSON ErrBody

instance Yesod TodoApp where
    approot = ApprootStatic "http://localhost:3000"
    errorHandler errorResponse = do
        $(logWarn) (append "Error Response: "
                            $ pack (show errorResponse))
        req <- waiRequest
        let
            errorText NotFound = (404, "Not Found", "Sorry, not found")
            errorText (InternalError msg) = (400, "Bad Request", msg)
            errorText (InvalidArgs m) = (400, "Bad Request", T.unwords m)
            errorText (PermissionDenied msg) = (403, "Forbidden", msg)
            errorText (BadMethod _) = (405, "Method Not Allowed", "Method not supported")
            (code, brief, full) = errorText errorResponse
            in sendResponseStatus
                (mkStatus code brief)
                $ RepJson $ toContent $ encode $ ErrBody (append "Error: " full) False
    
instance YesodPersist TodoApp where
    type YesodPersistBackend TodoApp = SqlBackend

    runDB f = do
        master <- getYesod
        let pool = connPool master
        runSqlPool f pool

postTodoR :: Handler String
postTodoR = do
    todo <- requireJsonBody
    _ <- runDB $ insert (todo :: Todo)
    sendResponseStatus (mkStatus 200 "") $ RepJson $ toContent $ (C.unpack $ encode todo)

getTodoR  :: Handler String 
getTodoR = do
    entries <- runDB $ selectList [] [Desc TodoOrder]
    return . C.unpack $ encode entries

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "crud" 10 -- create a new pool
    runSqlPersistMPool (runMigration migrateAll) pool
    manager <- newManager tlsManagerSettings -- create a new HTTP manager
    warp 3000 $ TodoApp pool manager -- start our server
