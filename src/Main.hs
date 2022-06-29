{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Generics.Sum.Any (AsAny (_As))
import Data.SOP (I (I), NP (Nil, (:*)))
import Data.Some
import Data.Time (UTCTime)
import Ema
import Ema.CLI qualified
import Ema.Route.Generic
import FPIndia.StaticRoute (StaticRoute (StaticRoute))
import FPIndia.StaticRoute qualified as SR
import Generics.SOP qualified as SOP
import Optics.Core (Prism', (%))
import System.FilePath ((</>))
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Route_Html HtmlRoute
  | Route_Static (StaticRoute UTCTime)
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes)
    via ( Route
            `WithSubRoutes` '[ HtmlRoute
                             , StaticRoute UTCTime
                             ]
        )
  deriving (IsRoute) via (Route `WithModel` Model)

instance HasSubModels Route where
  subModels m =
    I () :* I (modelFiles m) :* Nil

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

staticDir :: FilePath
staticDir = "static"

instance EmaSite Route where
  siteInput cliAct _ = do
    filesDyn <- SR.staticFilesDynamic staticDir
    pure $ Model cliAct <$> filesDyn
  siteOutput rp m = \case
    Route_Html r ->
      Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static (StaticRoute path) ->
      Ema.AssetStatic $ staticDir </> path

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    renderHead rp m
    renderBody rp m r

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp _m r = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    H.h1 ! A.class_ "text-3xl font-bold" $ "FPIndia WIP"
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
      A.href (fromString . toString $ Ema.routeUrl rp r')

renderHead :: Prism' FilePath Route -> Model -> H.Html
renderHead rp model = do
  H.meta ! A.charset "UTF-8"
  -- This makes the site mobile friendly by default.
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title "FPIndia"
  H.base ! A.href "/"
  -- H.link ! A.href (staticUrlTo rp "logo.svg") ! A.rel "icon"
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (modelCliAction m) (rp % (_As @"Route_Static")) (modelFiles m)

main :: IO ()
main = void $ Ema.runSite @Route ()
