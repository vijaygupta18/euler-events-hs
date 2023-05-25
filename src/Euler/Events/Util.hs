{-
 Copyright 2023-24, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under
 the terms of the GNU Affero General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your option)
 any later version. This program is distributed in the hope that it will be
 useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Affero General Public License for more details. You should have
 received a copy of the GNU Affero General Public License along with this
 program. If not, see <https://www.gnu.org/licenses/>.
-}

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
