module Data.DateTime.Extra where

import Prelude
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

getNow :: Effect DateTime
getNow = (unsafePartial fromJust <<< JSDate.toDateTime) <$> JSDate.now
