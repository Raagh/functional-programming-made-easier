module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test (returnString)

blondeHairStudents :: Int
blondeHairStudents = 
  let  s = 20
       b = 12
       r = 3
  in s - b - r

add :: forall t10. Semiring t10 => t10 -> t10 -> t10
add x y = x + y

factorial :: Int -> Int
factorial n = n * factorial (n - 1)

main :: Effect Unit
main = 
  log returnString   
