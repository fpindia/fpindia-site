{-# LANGUAGE UndecidableInstances #-}

module FPIndia.View.Util where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Model (Model (..))
import FPIndia.Route (HtmlRoute (..), Route (Route_Html))
import Optics.Core (Prism', (%))
import Text.Blaze.Html5 qualified as H

routeHref :: Prism' FilePath Route -> HtmlRoute -> H.AttributeValue
routeHref rp r = fromString . toString $ Ema.routeUrlWith Ema.UrlPretty rp (Route_Html r)

routeTitle :: HtmlRoute -> Text
routeTitle r = case r of
  HtmlRoute_About -> "About"
  HtmlRoute_ConnectWithUs -> "Connect"
  HtmlRoute_FpJobsInIndia -> "Jobs"
  HtmlRoute_Index -> "Home"
  HtmlRoute_PastEvents -> "Archive"
  HtmlRoute_Resources -> "Resources"
  HtmlRoute_UpcomingEvents -> "Events"

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)
