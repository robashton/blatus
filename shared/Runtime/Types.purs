module Pure.Runtime.Types where

import Prelude
import Foreign (readString)
import Simple.JSON (class ReadForeign, class WriteForeign, write)

newtype Empty
  = Empty Unit

empty :: Empty
empty = Empty unit

instance writeEmpty :: WriteForeign Empty where
  writeImpl _ = write ""

instance readEmpty :: ReadForeign Empty where
  readImpl f = empty <$ readString f

instance showEmpty :: Show Empty where
  show _ = "Empty"

instance eqEmpty :: Eq Empty where
  eq _ _ = true
