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


selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn
          "SELECT * FROM tools WHERE id = (?)"
          (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp


firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x


updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
  { lastReturned = date
  , timesBorrowed = 1 + timesBorrowed tool
  }


updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
                            \conn -> do
                              let q = mconcat ["UPDATE TOOLS SET "
                                              ,"lastReturned = ?, "
                                              ,"timesBorrowed = ? "
                                              ,"WHERE ID = ?;"]
                              execute conn q (lastReturned tool
                                             , timesBorrowed tool
                                             , toolId tool)
                              print "tool updated"


updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
                         \conn -> do
                           tool <- selectTool conn toolId
                           currentDay <- utctDay <$> getCurrentTime
                           let updatedTool = updateTool <$> tool
                                                        <*> pure currentDay
                           updateOrWarn updatedTool


checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" $
                  \conn -> do
                    execute conn
                      "DELETE FROM checkedout WHERE tool_id = (?);"
                      (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId
