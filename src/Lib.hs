module Lib
    ( someFunc
    ) where

import Data.List.Split
   ( splitOn )
import Data.Char
   ( digitToInt
   , isDigit
   , isAlphaNum
   )
import Datatypes

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | This function parses a 'date' field of the 'Entry' type
hasValidDate :: String -> Bool
hasValidDate str = and [ has10chars str
                       , has3fields str
                       , ymdLengthOK
                       , toDate
                       ]
   where has10chars  s = length s == 10
         splitedDate s = splitOn "-" s
         [y,m,d]       = splitOn "-" str
         has3fields  s = length (splitedDate s) == 3
         ymdLengthOK   = (length y == 4) && (length m == 2) && (length d == 2)
         allAreDigits  = all isDigit (concat [y,m,d])
         toDate        = allAreDigits
                           &&
                             (let toYear  = sum $ zipWith (*) [1000,100,10,1]
                                               $ map digitToInt y
                                  toMonth = sum $ zipWith (*) [10,1]
                                               $ map digitToInt m 
                                  toDay   = sum $ zipWith (*) [10,1]
                                               $ map digitToInt d 
                             in and [ toYear  > 1899
                                    , toYear  < 2201
                                    , toMonth > 0
                                    , toMonth < 13
                                    , toDay   > 0
                                    , toDay   < 32
                                    ])

