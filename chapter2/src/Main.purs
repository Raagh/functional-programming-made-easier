module Main where

import Prelude

import Data.String.Common (toLower, toUpper)
import Effect (Effect)
import Effect.Console (log)
import Data.String.CodePoints (length)

isSmall :: String -> Boolean
isSmall s = length s < 10 

isOddLength :: String -> Boolean
isOddLength s = length s `mod` 2 /= 0

appendIf :: (String -> Boolean) -> String -> String -> String
appendIf pred s append = if pred s then s <> append else s

show3 :: Int -> (Int -> (Int -> String))
show3 x y z = show x <> "," <> show y <> "," <> show z

toString :: Int -> String
toString n = show n

toArray :: String -> Array String
toArray s = [s]

intToStringArray :: Int -> Array String
intToStringArray = toArray <<< toString
-- intToStringArray n = toArray(toString n)

padLeft :: Char -> Int -> String -> String
padLeft _ _ x = x

zeroPad :: Int -> String -> String
zeroPad = padLeft '0' 

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

upperLower :: Int -> (String -> String)
upperLower n = if isEven n then toUpper else toLower

main :: Effect Unit
main = do 
  log $ upperLower 0 "this should be output in uppercase"
  log $ upperLower 1 "THIS SHOULD BE OUTPUT IN LOWERCASE"
--   log $ appendIf isSmall "Hello World" "!!!"  
--   log $ appendIf isOddLength "Hello World" "!!!"  
  

