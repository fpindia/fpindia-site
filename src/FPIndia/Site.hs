{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FPIndia.Site where

import Data.Default (def)
import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Lib.Extra.PandocRoute qualified as PR
import FPIndia.Jobs qualified as Jobs
import FPIndia.Model (Model (Model, modelStatic))
import FPIndia.Route (Route (..), StaticRoute)
import FPIndia.View (renderHtmlRoute)
import Optics.Core ((%))

instance EmaSite Route where
  siteInput cliAct () = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    markdownDyn <-
      siteInput @PR.PandocRoute cliAct $
        def
          { PR.argBaseDir = "markdown"
          , PR.argFormats = one ".md"
          }
    jobsDyn <- Jobs.jobsDynamic "jobs/jobs.csv"
    pure $ Model <$> staticRouteDyn <*> markdownDyn <*> jobsDyn
  siteOutput rp m = \case
    Route_Html r ->
      pure $ Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

runFPIndiaSite :: IO ()
runFPIndiaSite = Ema.runSite_ @Route ()
