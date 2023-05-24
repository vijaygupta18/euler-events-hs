module Euler.Events.Types.Timer where

import Data.Text (Text)


data MockedTimer  = MockedTimer Text
  deriving (Show, Read, Eq, Ord)

data Timer
  = Timer Text NativeTimer
  | MTimer MockedTimer
  deriving (Show, Read, Eq, Ord)

-- TODO
-- foreign import data NativeTimer :: Type
-- from Engineering.Middlewares.Monitoring
data NativeTimer = NativeTimer
  deriving (Show, Read, Eq, Ord)
