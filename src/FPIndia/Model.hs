module FPIndia.Model where

import Ema.Route.Lib.Extra.PandocRoute qualified as PR
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Jobs (Job)
import GHC.TypeLits (Symbol)

data Model = Model
  { modelStatic :: SR.Model
  , modelMarkdown :: PR.Model PandocExts
  , modelJobs :: [Job]
  }
  deriving stock (Show, Generic)

-- | The file types we want to monitor under ./markdown
type PandocExts :: [Symbol]
type PandocExts = '[".md"]
