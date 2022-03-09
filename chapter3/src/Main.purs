module Main where

import Prelude

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

main :: Effect Unit
main = do
  log "Hello, world!"
