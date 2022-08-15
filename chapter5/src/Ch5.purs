module Ch5 where

import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, show, (+), (-), (/=), (<), (==), (>=))
import Prim.RowList (Nil)

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

length' :: forall a. List a -> Int
length' l  = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil    = acc
  go acc (_:xs) = go (acc + 1) xs 

head :: forall  a. List a -> Maybe a
head Nil   = Nothing
head (x:_) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil    = Nothing
tail (_:xs) = Just xs

last :: forall a. List a -> Maybe a
last Nil = Nothing
last (_:xs) = head $ reverse xs

last' :: forall a. List a -> Maybe a
last' l = go Nothing l where
  go :: Maybe a -> List a -> Maybe a
  go prev Nil = prev
  go _ (x:xs) = go (Just x) xs 

last'' :: forall a. List a -> Maybe a
last'' Nil     = Nothing
last'' (x:Nil) = Just x
last'' (_:xs)  = last'' xs 

init :: forall a. List a -> Maybe (List a)
init l = go Nil l where
  go :: List a -> List a -> Maybe (List a)
  go _ Nil        = Nothing
  go prev (_:Nil) = Just (reverse prev)
  go prev (x:xs)  = go (x : prev) xs 

init' :: forall a. List a -> Maybe (List a)
init' Nil = Nothing
init'  l  = Just $ go l where
  go :: List a -> List a
  go Nil      = Nil
  go (_: Nil) = Nil
  go (x: xs)  = x : go xs

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil    = Nothing
uncons (x:xs) = Just { head: x, tail: xs }

index :: forall a. List a -> Int -> Maybe a
index Nil  _    = Nothing
index (x:_) 0   = Just x
index _ i | i < 0        = Nothing
index (_:xs) i           = index xs (i - 1)
-- index (x:xs)  i = if (i > length (x:xs)) then Nothing else index xs (i - 1) 
-- index (x:xs) i  
--   | i < 0 = Nothing
--   | i > length (x:xs) = Nothing
--   | otherwise = index xs (i - 1)

infixl 8 index as !!

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil   = Nothing
findIndex f l = go l 0 where
  go :: List a -> Int -> Maybe Int
  go Nil _ = Nothing
  go (x:xs) i = if (f x) then Just i else go xs (i + 1)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred l = go l 0 Nothing where
  go :: List a -> Int -> Maybe Int -> Maybe Int
  go Nil _ prev       = prev 
  go (x:xs) curr prev = if (pred x) 
                        then go xs (curr + 1) (Just curr) 
                        else go xs (curr + 1) prev

-- ESTA LA HIZO COPILOT
reverse' :: List ~> List
reverse' Nil = Nil
reverse' (x:xs) = snoc (reverse xs) x

reverse'' :: List ~> List
reverse'' Nil = Nil
reverse'' ol = go Nil ol where
  go :: forall a. List a -> List a -> List a
  go rl Nil    = rl
  go rl (x:xs) = go (x : rl) xs
  
concat :: forall a. List (List a) -> List a
concat Nil    = Nil
concat ll = go Nil ll where
  go l Nil = l

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
  log $ show $ length' (1 : 2 : 3 : Nil)
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ (last' Nil :: Maybe Unit)
  log $ show $ last' ("a" : "b" : "c" : Nil)
  log $ show $ (last'' Nil :: Maybe Unit)
  log $ show $ last'' ("a" : "b" : "c" : Nil)
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ init' (Nil :: List Unit)
  log $ show $ init' (1 : Nil)
  log $ show $ init' (1 : 2 : Nil)
  log $ show $ init' (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse' (1 : 2 : 3 : Nil)
  log $ show $ reverse'' (1 : 2 : 3 : Nil)
