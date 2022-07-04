module FPIndia.Model where

import Ema.Route.Lib.Extra.MarkdownRoute qualified as MR
import Ema.Route.Lib.Extra.StaticRoute qualified as SR

data Model = Model
  { modelStatic :: SR.Model
  , modelMarkdown :: MR.Model
  }
  deriving stock (Show, Generic)
