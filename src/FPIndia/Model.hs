module FPIndia.Model where

import Ema.Route.Lib.Extra.MarkdownRoute qualified as MR
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Jobs (Job)

data Model = Model
  { modelStatic :: SR.Model
  , modelMarkdown :: MR.Model
  , modelJobs :: [Job]
  }
  deriving stock (Show, Generic)
