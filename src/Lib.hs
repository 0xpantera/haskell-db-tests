module Lib
    ( addUser
    ) where

import Lib.Types
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



printUsers :: IO ()
printUsers = withConn "tools.db" $
              \conn -> do
                resp <- query_ conn "SELECT * FROM users;" :: IO [User]
                mapM_ print resp


printToolQuery :: Query -> IO ()
printToolQuery q = withConn  "tools.db" $
                     \conn -> do
                       resp <- query_ conn q :: IO [Tool]
                       mapM_ print resp


printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"


printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat ["select * from tools "
                                           ,"where id not in "
                                           ,"(select tool_id from checkedout);"]


printCheckedout :: IO ()
printCheckedout = printToolQuery $
                    mconcat ["select * from tools "
                            ,"where id in "
                            ,"(select tool_id from checkedout);"]


                    
