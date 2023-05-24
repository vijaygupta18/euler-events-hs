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
