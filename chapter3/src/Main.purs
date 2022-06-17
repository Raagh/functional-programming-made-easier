module Main where

import Prelude

import Data.Date (Date)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)

s :: String
s = "This is a multi-line\nwith embeded newlines"

s2 :: String
s2 = "This is a multi-line with continuations\
     \ at the end of the lines"

s3 :: String
s3 = """
  This is a multi-line that can contain quotes "" but \n will not be a newline """

unicodeStr :: String
unicodeStr = "This is a unicode character: \x00E9"

i :: Int
i = 42

i2 :: Int
i2 = 1 + 4

smallestInt :: Int
smallestInt = (-2147483648)

largestInt :: Int
largestInt = 2147483647

t :: Boolean
t = true

f :: Boolean
f = false

n :: Number
n = 1.0

smallestNumber :: Number
smallestNumber = (-5e-324)

largestNumber :: Number
largestNumber = 1.7976931348623157e+308

mta :: Array Number
mta = []

a :: Array Int
a = [1, 2, 3]

a2 :: Array String
a2 = ["abc", "123"]

aa :: Array (Array Int)
aa = [ [1, 2, 3], [4, 5], [6, 7, 8, 9] ]
 
r :: { firstName :: String, lastName :: String }
r = { firstName: "Joe", lastName: "Mama" }

type Person =
  { name :: String
  , age :: Int
  }

person :: Person
person = { name: "Jane Doe", age: 37 }

newPerson :: Person
newPerson = person { name = "Randy Cane" }

type Nested =
  { val :: Int
  , rec :: 
    { val2 :: Int
    , name :: String
    }
  }

type Id = String

type Message = { id :: Id, payload :: String }

data MyType = MyType

data Bool = True | False

mkTrue :: Bool
mkTrue = True

data FailureReason
  = InvalidSyntax
  | InvalidInput
  | AlreadyExists
  | NotFound
  | Other String

data FailureReason' a
  = InvalidSyntax'
  | InvalidInput'
  | AlreadyExists'
  | NotFound'
  | Other' a

failWithString :: FailureReason' String
failWithString = Other' "Because this function always fails"

type Error = { code :: Int, reason :: String }

failWithError :: FailureReason' Error
failWithError = Other' { code: -123, reason: "Because of reasons" }

data Triplet a b c = Triplet a b c

data StringTriplet = StringTriplet String Int Int

data StringStats = StringStats 
  { string :: String
  , length :: Int
  , vowelCount :: Int
  }

-- vowelCount :: String -> Int
-- vowelCount _ = 0
--
-- getStats :: String -> StringStats
-- getStats s = StringStats { string: s, length: length s, vowelCount: vowelCount s }

from :: StringTriplet -> StringStats
from (StringTriplet string length vowelCount) =
  StringStats
  { string: string
  , length: length
  , vowelCount: vowelCount
  }

to :: StringStats -> StringTriplet
to (StringStats { string, length, vowelCount }) =
  StringTriplet string length vowelCount

newtype FirstName = FirstName String
newtype MiddleName = MiddleName String
newtype LastName = LastName String

fullName :: FirstName -> MiddleName -> LastName -> String
fullName (FirstName first) (MiddleName middle) (LastName last) =  first <> " " <> middle <> " " <> last

-- this are an indefinity type recursive definition (there is no way to construct this type)
-- v :: Void
-- v = Void (Void(Void(Void(...))))
data Void = Void Void 

-- This one is a recursive defintion but it has an exit clause aka `TheEnd constructor`
data NeverEnding = NeverEnding NeverEnding | TheEnd

actuallyEnds :: NeverEnding
actuallyEnds = NeverEnding (NeverEnding (NeverEnding TheEnd))

data Maybe a = Just a | Nothing

data Person2 = Person2
  { name :: String,
    birthdate :: Date,
    deathdate :: Maybe Date
  }

-- Example creating a Person with no enddate, not sure how to create valid date
-- randomPerson :: Person2
-- randomPerson = Person2
--   { name: "Joe Mama",
--     birthdate: canonicalDate 1962 10 2,
--     deathdate: Nothing
--   }

-- We create a new person with the data of the previous one but with deathdate
-- deadPerson = randomPerson { deathdate = Just today }

add :: Int -> Int -> Int 
add x y = x + y

useAdd :: Maybe Int -> Maybe Int
useAdd y = 
  let x :: Int
      x = 10
    in
    case y of
      Just y -> Just (add x y)
      Nothing -> Nothing

data Either a b = Left a | Right b

data QueryError
  = DatabaseConnectionError String
  | InvalidQuery
  | NotFoundError

data Query = JustInt Int | NothingInt
 
query :: Query -> Either QueryError Int
query q =
  case q of
    JustInt a -> Right a
    NothingInt -> Left $ DatabaseConnectionError "DATABASE ERROR!"

hush :: forall a b. Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right x) = Just x

note :: forall a b. a -> Maybe b -> Either a b 
note err Nothing = Left err
note _ (Just x) = Right x

type Point = Tuple Int Int

data List a = Cons a (List a) | Nil
infixr 6 Cons as :

nums :: List Int
nums = 1 : 2 : 3 : Nil

myHead :: forall a. List a -> Maybe a
myHead Nil = Nothing
myHead (x:_) = Just x

myTail :: forall a. List a -> Maybe (List a)
myTail Nil = Nothing
myTail (_:xs) = Just xs

isNothing :: forall a. Maybe a -> Boolean
-- isNothing m = case m of
--   Nothing -> true
--   _       -> false
isNothing Nothing = true
isNothing _       = false

sum :: List Int -> Int
sum Nil    = 0
sum (x:xs) = x + sum xs

isEmpty :: forall a. Array a -> Boolean
-- isEmpty [] = true
-- isEmpty _  = false
isEmpty x = case x of
  [] -> true
  _  -> false

fromString :: String -> Boolean
fromString "true" = true
fromString _      = false

multiplyTwo :: Array Int -> Maybe Int
multiplyTwo [x, y] = Just (x * y)
multiplyTwo _      = Nothing

type Address = 
  { 
    street :: String,
    city :: String,
    state :: String,
    zip :: String
  }

type Employee = 
  {
  name :: String,
  jobTitle :: String,
  yearsAtCompany :: Int,
  address :: Address
  }

isCEO :: Employee -> Boolean
-- isCEO employee = employee.jobTitle == "CEO" 
isCEO { jobTitle } = jobTitle == "CEO"
  
isEmployeeFromCalifornia :: Employee -> Boolean
isEmployeeFromCalifornia { address: { state } } = state == "CA"


-- matches against all types that have an address field of type address
-- independent of what other fields they might have
-- weird syntax? why do you need to know that it has other fields? why does it matter?
isFromCalifornia :: forall r. { address :: Address | r } -> Boolean
isFromCalifornia { address: { state } } = state == "CA"

main :: Effect Unit
main = do
  -- log $ fullName (LastName "Ferraggi") (MiddleName "") (FirstName "Patricio") -- This throws error because fullName has parameters in the wrong order
  log $ fullName (FirstName "Patricio") (MiddleName "") (LastName "Ferraggi")
  -- log $ show $ isFromCalifornia { name : "", jobTitle: "", yearsAtCompany: 0, address: { street: "", city: "", zip: "",  state : "CA" } }
