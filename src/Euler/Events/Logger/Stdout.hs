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

module Euler.Events.Logger.Stdout where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor (($>))
import Euler.Events.Class
  ( ErrorText,
    EventPayload (toEvent),
    Logger (closeLogger, initLogger, logEvent, toLazyByteString),
  )
import Euler.Events.Types.Event (EventMetadata)
import qualified Data.ByteString as BS

data StdoutConfig
  = StdoutConfig

instance Logger StdoutConfig () where
  initLogger :: StdoutConfig -> IO (Either ErrorText ())
  initLogger _config = pure . Right $ ()
  toLazyByteString ::
    (EventPayload a) =>
    StdoutConfig ->
    () ->
    EventMetadata ->
    a ->
    BSL.ByteString
  toLazyByteString _config _logger metadata = encode . toEvent metadata
  logEvent ::
    (EventPayload a) =>
    StdoutConfig ->
    () ->
    EventMetadata ->
    a ->
    IO (Maybe ErrorText)
  logEvent config logger metadata eventPayload =
    (BS.putStr . BSL.toStrict . (<> "\n") . toLazyByteString config logger metadata $ eventPayload)
      $> Nothing
  closeLogger :: () -> IO (Maybe ErrorText)
  closeLogger () = pure Nothing
