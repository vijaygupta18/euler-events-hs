module Euler.Events.Util where

import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Time.Clock (UTCTime, diffTimeToPicoseconds, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- import           Proto3.Suite          (Enumerated (Enumerated))

fromInt :: Integral a => a -> Int64
fromInt = fromIntegral

-- fromSumType :: a -> Enumerated a
-- fromSumType = Enumerated . Right

fromText :: Text -> Text.Lazy.Text
fromText = fromStrict

fromUTCTime :: UTCTime -> Int64
fromUTCTime = fromInt . utcTimeToNanos

utcTimeToNanos :: UTCTime -> Integer
utcTimeToNanos t =
  diffTimeToPicoseconds (realToFrac (diffUTCTime t (posixSecondsToUTCTime 0)))
    `div` 1000

tshow :: Show a => a -> Text
tshow = pack . show
