{-# LANGUAGE UndecidableInstances #-}

module FPIndia.View.Util where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Encoder (applyRouteEncoder)
import Ema.Route.Lib.Extra.MarkdownRoute qualified as MR
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Model (Model (..))
import FPIndia.Route (HtmlRoute (..), Route (Route_Html))
import Optics.Core (Prism', (%))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

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

-- | Render a Markdown file inside ./markdown directory
renderMarkdown :: Model -> String -> H.Html
renderMarkdown m fp =
  H.div ! A.class_ ("prose " <> proseStyle) $ do
    renderMarkdown' m fp
  where
    -- Style Pandoc generated HTML en masse here.
    -- See https://tailwindcss.com/docs/typography-plugin
    proseStyle = "prose-a:underline prose-a:decoration-indigo-700 prose-a:decoration-wavy prose-a:decoration-2 hover:prose-a:decoration-4"

-- | Like `renderMarkdown` but without the prose styling
renderMarkdown' :: Model -> String -> H.Html
renderMarkdown' m fp =
  let model = modelMarkdown m
      rp = applyRouteEncoder (routeEncoder @MR.MarkdownRoute) model
      (pandoc, render) = siteOutput rp model $ fromString fp
   in renderRawHtml $ MR.unMarkdownHtml $ render pandoc
  where
    renderRawHtml =
      H.unsafeLazyByteString . encodeUtf8
