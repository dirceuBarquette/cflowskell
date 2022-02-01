module LibCflowskell
    ( 
    ) where

import Data.List.Split
   ( splitOn )
import Data.Char
   ( digitToInt
   , isDigit
   , isAlphaNum
   , isSpace
   )
import Datatypes

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- getEntryFromUser :: IO ()
-- getEntryFromUser = do
--    if 

getNonEmptyChar :: IO Char
getNonEmptyChar = do
   v <- getChar
   if isSpace v then getNonEmptyChar else return v

getOption :: String -> IO Int
getOption lang = do 
   x <- getNonEmptyChar
   if isDigit x  then
      return (digitToInt x)
   else
      do --newline
         putStrLn $ if null lang then "Dígito inválido!" else "Invalid digit!"
         return 9

-- | This function feed a list of the 'Entry' type 
toEntriesList :: Entry -> [Entry] -> [Entry]
toEntriesList e el = el ++ [e]

-- | This function parses th 'description' field of the 'Entry' type
hasValidDescr :: String -> Bool
hasValidDescr str = length str < 80 && not (null str)

-- | This function parses the 'opType' field of the 'Entry' type
hasValidOper :: String -> Bool
hasValidOper str | str `elem` oPs = True
                 | otherwise      = False
                    where oPs = ["Deposit", "Withdraw"]

-- | This function parses the 'value' field of the 'Entry' type
hasValidValue :: String -> Bool
hasValidValue str = all isDigit (concat $ splitOn "." str)
                     && not (null str)

-- | This function parses the 'date' field of the 'Entry' type
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

