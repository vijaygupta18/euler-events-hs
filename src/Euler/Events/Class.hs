{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Events.Class where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Map(Map)
import Data.Text (Text)
import Euler.Events.Constants (eventLibraryVersion)
import Euler.Events.Types.Event (Event (Event), EventMetadata, EventType)
import qualified Euler.Events.Types.Event as Event
import Euler.Events.Types.Metric (MetricOperation, MetricResult)
import Network.Wai (Middleware)

type ErrorText = Text

class
  (ToJSON a, FromJSON a) =>
  EventPayload a
  where
  toEvent :: EventMetadata -> a -> Event a
  toEvent' :: EventType -> EventMetadata -> a -> Event a
  toEvent' eventType metadata eventPayload =
    Event
      { Event.metadata = metadata,
        Event.eventLibraryVersion = eventLibraryVersion,
        Event.event = eventType,
        Event.message = eventPayload
      }

class Logger config logger where
  initLogger :: config -> IO (Either ErrorText logger)
  toLazyByteString ::
    EventPayload a => config -> logger -> EventMetadata -> a -> ByteString
  logEvent ::
    EventPayload a =>
    config ->
    logger ->
    EventMetadata ->
    a ->
    IO (Maybe ErrorText)
  closeLogger :: logger -> IO (Maybe ErrorText)

class
  MetricLogger config logger metric
    | config -> metric,
      metric -> logger,
      logger -> config
  where
  initMetricLogger :: config -> IO logger
  metricEvent ::
    logger ->
    MetricOperation metric ->
    IO (Either ErrorText (MetricResult metric))
  emitMetricIO :: logger -> MetricOperation metric -> IO ()
  emitMetricIO logger = void . metricEvent logger
  instrumentApp :: metric -> Map String String -> (Text -> Text) -> Middleware
