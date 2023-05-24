{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Euler.Events.MetricApi.Extra
  (
  -- * Ready gauge
    ReadyHandler (..)
  , Ready(..)
  , mkReadyHandler

  -- * Observe request time histogram
  , histogramRequestTime
  , sendHistorgam
  ) where

import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Euler.Events.MetricApi.MetricApi
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Middleware)
import qualified Network.Wai as Wai
import System.Posix.Process (getProcessID)
import Data.Time.Clock.System (SystemTime(..),getSystemTime)


-------------------------------------------------------------------------------
-- Move to another module.
-- Land metric stuff on new api
-------------------------------------------------------------------------------

-- Gauge metric to fix state of start and down of an application
data Ready
  = ReadyUp
  | ReadyDown
  deriving stock Show

data ReadyHandler = ReadyHandler
  { setReadyGauge :: Ready -> IO ()
  }

mkReadyHandler :: IO ReadyHandler
mkReadyHandler = do
  let up = (gauge #up) .& build
  let collection = up .> MNil
  metrics <- register collection
  let go = setGauge $ metrics </> #up
  pure $ ReadyHandler $ \case
      ReadyUp   -> go 1
      ReadyDown -> go 0

-- Histogram to observe request time
sendHistorgam :: Double
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO ()
sendHistorgam
  latency
  status
  method
  path
  host
  eulerInstance
  pid
  merchantId = do
    let euler_http_request_duration = histogram'
          #euler_http_request_duration histHelp
            .& lbl @"status_code" @Text
            .& lbl @"method" @Text
            .& lbl @"path" @Text
            .& lbl @"host" @Text
            .& lbl @"eulerInstance" @Text
            .& lbl @"pid" @Text
            .& lbl @"merchant_id" @Text
            .& build
    let collectionHistogram = euler_http_request_duration .> MNil
    coll <- register collectionHistogram
    observe (coll </> #euler_http_request_duration)
       latency
       status
       method
       path
       host
       eulerInstance
       pid
       merchantId

histHelp :: Text
histHelp = "duration histogram of http responses labeled with: status_code, method, path, host, eulerInstance, pid, merchant_id"

observeSecondsNew ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Map.Map String String ->
  SystemTime ->
  SystemTime ->
  IO ()
observeSecondsNew status method path merchantId envVars start end = do
  let latency
        = fromIntegral $ (systemNanoseconds end) - (systemNanoseconds start)
  let eulerInstance = T.pack <$> Map.lookup "HOSTNAME" envVars
  let host = Nothing
  processId <- getProcessID
  let pid = T.pack . show <$> Just processId
  sendHistorgam
    latency
    (fromMaybe "" status)
    (fromMaybe "" method)
    (fromMaybe "" path)
    (fromMaybe "" host)
    (fromMaybe "" eulerInstance)
    (fromMaybe "" pid)
    (fromMaybe "" merchantId)

-- Previous 'instrumentApp'
histogramRequestTime :: Map.Map String String -> (Text -> Text) -> Middleware
histogramRequestTime envVars normalizer app req respond = do
  start <- getSystemTime
  app req $ \res -> do
    end <- getSystemTime
    let method = Just $ decodeUtf8 (Wai.requestMethod req)
    let status
          = Just
          $ T.pack
          $ show
          $ HTTP.statusCode
          $ Wai.responseStatus res
    let path
          = fmap normalizer
          $ Just
          $ T.intercalate "/"
          $ "" : filter (\e -> T.length e /= 0) (Wai.pathInfo req)
    -- TODO  factor out headers from Server.hs get rid of the literal
    let merchantId =
          decodeUtf8
            <$> ( fmap snd <$> find (\(h, _) -> h == "x-jp-merchant-id") $
                    Wai.responseHeaders res
                )
    observeSecondsNew status method path merchantId envVars start end
    respond res
