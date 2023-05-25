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
