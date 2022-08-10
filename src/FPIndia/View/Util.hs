{-# LANGUAGE UndecidableInstances #-}

module FPIndia.View.Util where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Lib.Extra.PandocRoute qualified as PR
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
  HtmlRoute_ConnectWithUs -> "Connect"
  HtmlRoute_FpJobsInIndia -> "Jobs"
  HtmlRoute_Index -> "Home"
  HtmlRoute_Events -> "Events"
  HtmlRoute_Resources -> "Resources"

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)

-- | Render a Markdown file inside ./markdown directory
renderMarkdown :: Model -> String -> H.Html
renderMarkdown m fp =
  H.div ! A.class_ ("prose max-w-none " <> proseStyle) $ do
    renderMarkdown' m fp
  where
    -- Style Pandoc generated HTML en masse here.
    -- See https://tailwindcss.com/docs/typography-plugin
    proseStyle = "rounded p-4 text-stone-800 prose-a:underline prose-a:decoration-rose-600 prose-a:decoration-solid prose-a:decoration-1 hover:prose-a:decoration-2"

-- | Like `renderMarkdown` but without the prose styling
renderMarkdown' :: HasCallStack => Model -> String -> H.Html
renderMarkdown' m fp =
  case PR.lookupPandocRoute (modelMarkdown m) (fromString fp) of
    Nothing -> error $ "renderMarkdown: not a Pandoc ext: " <> toText fp
    Just (pandoc, render) ->
      renderRawHtml $ PR.unPandocHtml $ render pandoc
  where
    renderRawHtml =
      H.unsafeLazyByteString . encodeUtf8
