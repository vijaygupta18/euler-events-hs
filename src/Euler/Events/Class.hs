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
