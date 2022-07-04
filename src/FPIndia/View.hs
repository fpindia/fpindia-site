{-# LANGUAGE UndecidableInstances #-}

module FPIndia.View where

import FPIndia.Model (Model)
import FPIndia.Route (HtmlRoute (..), Route)
import FPIndia.View.Util (routeHref, routeTitle, staticRouteUrl)
import Optics.Core (Prism')
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    H.html ! A.lang "en" $ do
      H.head $ do
        renderHead rp m r
      H.body $ do
        renderBody rp m r

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    renderNavbar rp
    H.h1 ! A.class_ "text-3xl font-bold" $ H.toHtml $ routeTitle r
    H.img ! A.src (staticRouteUrl rp model "logo.png") ! A.class_ "w-32" ! A.alt "FPIndia Logo"
    case r of
      HtmlRoute_Index -> do
        "Home page"
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
    renderFooter

renderNavbar :: Prism' FilePath Route -> H.Html
renderNavbar rp =
  H.nav ! A.class_ "w-full h-1/4 text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ universe $ \r -> renderURL (H.toHtml $ routeTitle r) (routeHref rp r)
  where
    renderURL menuItem path = H.a ! A.href path ! A.class_ "bg-rose-300 rounded p-2" $ menuItem

renderFooter :: H.Html
renderFooter = do
  H.footer
  ! A.class_ "w-full h-10 bg-rose-300 rounded p-2 border-t-2 border-white fixed left-0 bottom-0 flex justify-center items-center text-white text-1xl"
  $ do
    H.pre "Made with " ! A.class_ "text-black"
    H.a ! A.class_ "text-red-500 hover" ! A.href "https://ema.srid.ca/" $ "Ema"

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  -- This makes the site mobile friendly by default.
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Functional Programming India"
  H.base ! A.href "/"
  -- H.script ! A.src (staticRouteUrl rp model "main.js") $ ""
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")
