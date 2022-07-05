module Ch5 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show)


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
applyFlipped x f =

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
