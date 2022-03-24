module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.String.CodeUnits (length)


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

r2 :: Person
r2 = { name: "Jane Doe", age: 37 }

r3 :: Person
r3 = r2 { name = "Randy Cane" }

type Nested =
  { val :: Int
  , rec :: 
    { val2 :: Int
    , name :: String
    }
  }

type Id = String

type Message = { id :: Id, payload :: String }

data Bool = True | False

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

type StringStats = Triplet String Int Int

getStats :: String -> StringStats
getStats x = s (length s) (vowelCount s)

main :: Effect Unit
main = do
  log "Hello, world!"
