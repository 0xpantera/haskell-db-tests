module Lib
    ( addUser
    ) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time


withConn :: String -> (Connection -> IO()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn


addUser :: String -> IO ()
addUser userName = withConn "tools.db" $
                    \conn -> do
                      execute conn "INSERT INTO users (username) VALUES (?)"
                        (Only userName)
                      print "user added"


checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                          \conn -> do
                            execute conn
                              "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
                               (userId,toolId)



