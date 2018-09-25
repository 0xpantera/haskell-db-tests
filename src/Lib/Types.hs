module Lib.Types where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time


data Tool = Tool
  { toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int
  }

instance Show Tool where
  show tool = mconcat [ show $ toolId tool
                      , ".) "
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed: "
                      , show $ timesBorrowed tool
                      , "\n"]



instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field


data User = User
  { userId :: Int
  , userName :: String
  }

instance Show User where
  show tool = mconcat [ show $ userId user
                      , ".) "
                      , userName user]           


instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 
