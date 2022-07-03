{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
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
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , WithSubRoutes '[HtmlRoute, StaticRoute]
             ]
        )

type StaticRoute = SR.StaticRoute "static"

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_About
  | HtmlRoute_UpcomingEvents
  | HtmlRoute_PastEvents
  | HtmlRoute_ConnectWithUs
  | HtmlRoute_FpJobsInIndia
  | HtmlRoute_Resources
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            HtmlRoute
            '[ WithSubRoutes
                '[ FileRoute "index.html"
                 , FileRoute "about.html"
                 , FileRoute "upcoming.html"
                 , FileRoute "past.html"
                 , FileRoute "connect.html"
                 , FileRoute "jobs.html"
                 , FileRoute "resources.html"
                 ]
             ]
        )

data Model = Model
  { modelStatic :: SR.Model
  }
  deriving stock (Eq, Show, Generic)

instance EmaSite Route where
  siteInput cliAct () = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    pure $ Model <$> staticRouteDyn
  siteOutput rp m = \case
    Route_Html r ->
      Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    H.html ! A.lang "en" $ do
      H.head $ do
        renderHead rp m r
      H.body $ do
        renderBody rp m r

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

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    renderNavbar rp
    H.h1 ! A.class_ "text-3xl font-bold" $ "FPIndia WIP"
    H.img ! A.src (staticRouteUrl rp model "logo.png") ! A.class_ "w-32" ! A.alt "FPIndia Logo"
    case r of
      HtmlRoute_Index -> do
        "FP India"
      HtmlRoute_About -> do
        "You are on the about page."
        H.div $ H.p "We are a community and a meetup group for Functional Programming language enthusiasts in India. You can join and participate in the online events even if you are somewhere else. We organise regular meetups, events, webinars, and workshops, all centered around Functional Programming and related technologies. All skill levels from novices to gods of category theory are welcome."
      HtmlRoute_UpcomingEvents -> do
        "You are on the upcoming events page."
      HtmlRoute_PastEvents -> do
        "You are on the past events page."
      HtmlRoute_ConnectWithUs -> do
        "you are on the Connect with us page."
      HtmlRoute_FpJobsInIndia -> do
        "you are on the FP jobs in india page."
      HtmlRoute_Resources -> do
        "you are on the resources page."

renderNavbar :: Prism' FilePath Route -> H.Html
renderNavbar rp =
  H.nav ! A.class_ "w-full h-1/4 text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ universe $ \r -> renderURL (H.toHtml $ routeTitle r) (routeHref rp r)
  where
    renderURL menuItem path = H.a ! A.href path ! A.class_ "bg-rose-300 rounded p-2" $ menuItem

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  -- This makes the site mobile friendly by default.
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Functional Programming India"
  H.base ! A.href "/"
  -- H.script ! A.src (staticRouteUrl rp model "main.js") $ ""
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)

main :: IO ()
main = Ema.runSite_ @Route ()
