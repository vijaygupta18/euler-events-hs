module Euler.Events.Constants where

import Data.Text (Text)

kafkaBrokerAddress :: Text
kafkaBrokerAddress = "localhost:9092"

kafkaTimeout :: Int
kafkaTimeout = 10000

kafkaTargetTopic :: Text
kafkaTargetTopic = "test"

eventLibraryVersion :: Text
eventLibraryVersion = "0.0.2"
