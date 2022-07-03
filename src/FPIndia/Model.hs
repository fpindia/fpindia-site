module FPIndia.Model where

import Ema.Route.Lib.Extra.StaticRoute qualified as SR

data Model = Model
  { modelStatic :: SR.Model
  }
  deriving stock (Eq, Show, Generic)
