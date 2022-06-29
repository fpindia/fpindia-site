{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Generics.Sum.Any (AsAny (_As))
import Data.SOP (I (I), NP (Nil, (:*)))
import Data.Some (Some)
import Data.Time (UTCTime)
import Ema
import Ema.CLI qualified
import Ema.Route.Generic
import FPIndia.StaticRoute qualified as SR
import Generics.SOP qualified as SOP
import Optics.Core (Prism', (%))
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes)
    via ( Route
            `WithSubRoutes` '[ HtmlRoute
                             , StaticRoute
                             ]
        )
  deriving (IsRoute) via (Route `WithModel` Model)

instance HasSubModels Route where
  subModels m =
    I () :* I (modelFiles m) :* Nil

type StaticRoute = SR.StaticRoute "static" UTCTime

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_About
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes)
    via ( HtmlRoute
            `WithSubRoutes` [ FileRoute "index.html"
                            , FileRoute "about.html"
                            ]
        )
  deriving
    (HasSubModels, IsRoute)
    via (HtmlRoute `WithModel` ())

data Model = Model
  { modelCliAction :: Some Ema.CLI.Action
  , modelFiles :: Map FilePath UTCTime
  }
  deriving stock (Eq, Show)

instance EmaSite Route where
  siteInput cliAct () = do
    filesDyn <- siteInput @StaticRoute cliAct ()
    pure $ Model cliAct <$> filesDyn
  siteOutput rp m = \case
    Route_Html r ->
      Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelFiles m) r

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    renderHead rp m
    renderBody rp m r

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    H.h1 ! A.class_ "text-3xl font-bold" $ "FPIndia WIP"
    H.img ! A.src (staticRouteUrl rp model "logo.png") ! A.class_ "w-32"
    case r of
      HtmlRoute_Index -> do
        "You are on the index page. "
        routeElem HtmlRoute_About "Go to About"
      HtmlRoute_About -> do
        routeElem HtmlRoute_Index "Go to Index"
        ". You are on the about page. "
  where
    routeElem r' w = do
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref (Route_Html r') $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrlWith Ema.UrlPretty rp r')

renderHead :: Prism' FilePath Route -> Model -> H.Html
renderHead rp model = do
  H.meta ! A.charset "UTF-8"
  -- This makes the site mobile friendly by default.
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title "FPIndia"
  H.base ! A.href "/"
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (modelCliAction m) (rp % (_As @"Route_Static")) (modelFiles m)

main :: IO ()
main = void $ Ema.runSite @Route ()
