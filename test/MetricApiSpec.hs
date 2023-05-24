{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MetricApiSpec where

import Euler.Events.MetricApi.Extra
import Euler.Events.MetricApi.MetricApi
import Euler.Events.Network

import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Test.Hspec (Spec, beforeAll, describe, hspec, it, runIO, shouldBe)



spec :: Spec
spec = runIO $ bracket (async runMetricServer) cancel $ \_ -> hspec $
 beforeAll (register collection) $

  describe "Test the MetricApi." $ do
    it "Check metrics are empty" $ \_ -> do
      respBody <- getRespBody requestMetric
      -- traceTest respBody "isEmpty ---------"
      "g1 0.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc one vector metrics" $ \coll -> do
      inc (coll </> #c1) 42
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c1 --------------"
      "c1{foo=\"42\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Add two vector metrics" $ \coll -> do
      add (coll </> #c2) 2 3 True
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c2 add --------------"
      "c2{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics" $ \coll -> do
      inc (coll </> #c3) 3 True
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c3 inc --------------"
      "c3{foo=\"3\",bar=\"True\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc gauge metrics" $ \coll -> do
      incGauge (coll </> #g1)
      respBody <- getRespBody requestMetric
      -- traceTest respBody "g1 --------------"
      "g1 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics 2 times" $ \coll -> do
      inc (coll </> #c4) 3 True
      inc (coll </> #c4) 3 True
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c4 inc --------------"
      "c4{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics 2 times with different values" $ \coll -> do
      inc (coll </> #c5) 3 True
      inc (coll </> #c5) 3 False
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c5 inc --------------"
      and [ "c5{foo=\"3\",bar=\"True\"} 1.0" `BS.isInfixOf` respBody
          , "c5{foo=\"3\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True

    it "Reg two vector metrics 2 times" $ \coll -> do
      inc (coll </> #c6) 3 True
      coll2 <- register collection
      inc (coll2 </> #c6) 3 True
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c6 inc --------------"
      "c6{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "increment many metrics" $ \coll -> do
      inc (coll </> #c7) 7 False
      inc (coll </> #c8) 8 False
      inc (coll </> #c9) 9 False
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c7 inc --------------"
      and [ "c7{foo=\"7\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          , "c8{foo=\"8\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          , "c9{foo=\"9\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True

    it "Register metrics sequentually" $ \coll -> do
      inc (coll </> #c10) 10 False
      coll2 <- register collection2
      inc (coll2 </> #c11) 11 False
      inc (coll </> #c10) 10 False
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c7 inc --------------"
      and [ "c10{foo=\"10\",bar=\"False\"} 2.0" `BS.isInfixOf` respBody
          , "c11{foo=\"11\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True

    it "Check showing of text types" $ \coll -> do
      inc (coll </> #c12) "text" "string" "bs" True
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c12 inc --------------"
      "c12{foo=\"text\",bar=\"string\",bin=\"bs\",buz=\"True\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Set gauge metrics (Ready)" $ \_ -> do
      metric <- mkReadyHandler
      metric.setReadyGauge ReadyUp
      metric.setReadyGauge ReadyDown
      respBody <- getRespBody requestMetric
      -- traceTest respBody "up --------------"
      "up 0.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Check help is empty" $ \coll -> do
      inc (coll </> #c13) 3 True
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c13 help --------------"
      "# HELP c13 \n# TYPE c13 counter" `BS.isInfixOf` respBody `shouldBe` True

    it "Observe histogram" $ \coll -> do
      observe (coll </> #o14) 3 4
      observe (coll </> #o14) 3 1
      observe (coll </> #o14) 3 1
      respBody <- getRespBody requestMetric
      -- traceTest respBody "c14 histogram --------------"
      "o14_bucket{foo=\"1\",le=\"30.0\"} 2" `BS.isInfixOf` respBody `shouldBe` True

    it "Observe histogram with 7 vector" $ \_ -> do
      sendHistorgam
        3 "status_code" "method" "path" "host" "eulerInstance" "pid" "merchant_id"
      sendHistorgam
        3 "status_code" "method" "path" "host" "eulerInstance" "pid" "merchant_id"
      sendHistorgam
        3 "status_code" "method" "path" "host" "eulerInstance" "pid" "merchant_id"
      respBody <- getRespBody requestMetric
      let isTest = "euler_http_request_duration_sum{status_code=\"status_code\",method=\"method\",path=\"path\",host=\"host\",eulerInstance=\"eulerInstance\",pid=\"pid\",merchant_id=\"merchant_id\"} 3.0" `BS.isInfixOf` respBody
      case isTest of
        False -> do
          -- traceTest respBody "c14 histogram --------------"
          pure ()
        True -> isTest `shouldBe` True




c1 = counter' #c1 "help"
  .& lbl @"foo" @Int
  .& build

c2 = counter' #c2 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c3 = counter' #c3 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

g1 = gauge' #g1 "help"
      .& build

c4 = counter' #c4 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c5 = counter' #c5 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c6 = counter' #c6 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c7 = counter' #c7 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c8 = counter' #c8 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c9 = counter' #c9 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c10 = counter' #c10 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c11 = counter' #c11 "help"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c12 = counter' #c12 "help"
      .& lbl @"foo" @Text
      .& lbl @"bar" @String
      .& lbl @"bin" @ByteString
      .& lbl @"buz" @Bool
      .& build

c13 = counter #c13
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

o14 = histogram' #o14 "help"
  .& lbl @"foo" @Int
  .& build

-- collection of metrics, prevents from ambiguos metric names
collection =
     c1
  .> c2
  .> c3
  .> g1
  .> c4
  .> c5
  .> c6
  .> c7
  .> c8
  .> c9
  .> c10
  .> c12
  .> c13
  .> o14
  .> MNil

collection2 =
      c11
  .> MNil

-- collection3 = collection <> collection2
