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

module Euler.Events.Network where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Either.Combinators (fromRight')
import Network.HTTP.Simple
  ( Request,
    Response,
    defaultRequest,
    getResponseBody,
    httpBS,
    setRequestPort,
  )
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Prometheus as Prometheus

makeReq :: Request -> IO (Either SomeException (Response BS.ByteString))
makeReq req = try $ httpBS req

checkInfixes :: [BS.ByteString] -> BS.ByteString -> Bool
checkInfixes subStrs str = and ((`BS.isInfixOf` str) <$> subStrs)

getRespBody :: Request -> IO BS.ByteString
getRespBody req = getResponseBody . fromRight' <$> makeReq req

port :: Int
port = 9999

requestMetric :: Request
requestMetric = setRequestPort port defaultRequest

runMetricServer :: IO ()
runMetricServer = run port (Prometheus.metricsApp Nothing)

traceTest :: BS.ByteString -> String -> IO ()
traceTest resp str = do
  putStrLn ""
  putStrLn str
  BS.putStr resp

_ghcMetrics :: [BS.ByteString]
_ghcMetrics =
  [ "ghc_gcs_total",
    "ghc_major_gcs_total",
    "ghc_allocated_bytes_total",
    "ghc_max_live_bytes",
    "ghc_max_large_objects_bytes",
    "ghc_max_compact_bytes",
    "ghc_max_slop_bytes",
    "ghc_max_mem_in_use_bytes",
    "ghc_cumulative_live_bytes_total",
    "ghc_copied_bytes_total",
    "ghc_par_copied_bytes_total",
    "ghc_cumulative_par_max_copied_bytes_total",
    "ghc_mutator_cpu_seconds_total",
    "ghc_mutator_elapsed_seconds_total",
    "ghc_gc_cpu_seconds_total",
    "ghc_gc_elapsed_seconds_total",
    "ghc_cpu_seconds_total",
    "ghc_elapsed_seconds_total",
    "ghc_gcdetails_gen",
    "ghc_gcdetails_threads",
    "ghc_gcdetails_allocated_bytes",
    "ghc_gcdetails_live_bytes",
    "ghc_gcdetails_large_objects_bytes",
    "ghc_gcdetails_compact_bytes",
    "ghc_gcdetails_slop_bytes",
    "ghc_gcdetails_mem_in_use_bytes",
    "ghc_gcdetails_copied_bytes",
    "ghc_gcdetails_par_max_copied_bytes",
    "ghc_gcdetails_sync_elapsed_seconds",
    "ghc_gcdetails_cpu_seconds",
    "ghc_gcdetails_elapsed_seconds"
  ]

procMetrics :: [BS.ByteString]
procMetrics =
  [ "process_open_fds",
    "process_max_fds",
    "process_heap_size_bytes",
    "process_cpu_user_seconds_total",
    "process_cpu_system_seconds_total",
    "process_cpu_seconds_total",
    "process_start_time_seconds",
    "process_virtual_memory_bytes",
    "process_resident_memory_bytes"
  ]

_ghcProcMetrics :: [BS.ByteString]
_ghcProcMetrics = _ghcMetrics ++ procMetrics
