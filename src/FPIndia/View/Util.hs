{-# LANGUAGE UndecidableInstances #-}

module FPIndia.View.Util where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Model (Model (..))
import FPIndia.Route (HtmlRoute (..), Route (Route_Html))
import Optics.Core (Prism', (%))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

routeElem :: Prism' FilePath Route -> HtmlRoute -> H.Html -> H.Html
routeElem rp r w = do
  H.a ! A.class_ "text-red-500 hover:underline" ! A.href (routeHref rp r) $ w

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
