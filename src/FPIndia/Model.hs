module FPIndia.Model where

import Ema.Route.Lib.Extra.PandocRoute qualified as PR
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Jobs (Job)

data Model = Model
  { modelStatic :: SR.Model
  , modelMarkdown :: PR.Model
  , modelJobs :: [Job]
  }
  deriving stock (Generic)
