module Ch5 where

import Data.List (List(..), reverse, (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (+))

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

flip' :: forall a b c. (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x

-- flip = (f) => (x, y) => f y x 

const :: forall a b. a -> b -> a
const x _ = x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b 
applyFlipped x f = flip apply x f

applyFlipped' :: forall a b. a -> (a -> b) -> b
applyFlipped' = flip apply

infixl 1 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean 
null Nil = true
null _   = false

snoc :: forall a. List a -> a -> List a
snoc l x = reverse (Cons x (reverse l))

snoc' :: forall a. List a -> a -> List a
snoc' Nil x      = x : Nil
snoc' (y:ys) x = y : (snoc ys x) 

length :: forall a. List a -> Int
length Nil    = 0
length (_:xs) = 1 + length xs

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("1": Nil)
  log $ show $ snoc' (1 : 2 : Nil) 3
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length (1 : 2 : 3 : Nil)
