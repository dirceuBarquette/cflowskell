module Datatypes where

import Data.Time (Day)

data Op = Deposit | Withdraw deriving (Show, Enum)

data Entry = E { date        :: Day
               , oper        :: Op
               , description :: String
               , value       :: Double
               } deriving (Show)

