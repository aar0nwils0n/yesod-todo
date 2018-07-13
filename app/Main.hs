module Main where

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit (Manager, newManager)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool
    )

import Control.Monad.Logger (runStdoutLoggingT)
import YesodInstance

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "crud" 10 -- create a new pool
    runSqlPersistMPool (runMigration migrateAll) pool
    manager <- newManager tlsManagerSettings -- create a new HTTP manager
    warp 3000 $ TodoApp pool manager -- start our server
