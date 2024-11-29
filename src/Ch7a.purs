
module Ch7a where

import Prelude ( Unit, show, discard, (==), ($), (<), (>), (<=), (>=) )

import Data.Eq ( class Eq )
import Data.Generic.Rep ( class Generic )
import Data.Ord ( class Ord, Ordering (..), compare )
import Data.Show.Generic ( genericShow )
import Effect ( Effect )
import Effect.Console ( log )

data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance ordMaybe :: Ord a => Ord (Maybe a)
derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

test :: Effect Unit
test = do
  log "------------------------"
  log $ show $ Just 5 == Just 5
  log $ show $ Nothing   == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log $ show $ Just 5 >= Just 5
  log $ show $ (Nothing :: Maybe Unit)
  log $ show $ Just 5
