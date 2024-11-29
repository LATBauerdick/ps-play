module Ch7b where

import Prelude ( Unit )

import Effect ( Effect )
import Effect.Console ( log )

newtype CSV = CSV String

class ToCSV a where
  toCSV :: a -> CSV a

test :: Effect Unit
test = do
  log "------------------------"
