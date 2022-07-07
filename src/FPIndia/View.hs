{-# LANGUAGE UndecidableInstances #-}

module FPIndia.View where

import FPIndia.Jobs qualified as Jobs
import FPIndia.Model (Model (modelJobs))
import FPIndia.Route (HtmlRoute (..), Route)
import FPIndia.View.Util (renderMarkdown, routeHref, routeTitle, staticRouteUrl)
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
      H.body ! A.class_ "bg-slate-200 text-stone-900" $ do
        renderBody rp m r

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do 
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    renderNavbar rp
    H.h1 ! A.class_ "text-3xl font-bold" $ H.toHtml $ routeTitle r
    H.img ! A.src (staticRouteUrl rp model "fpindia-logo.png") ! A.class_ "w-32" ! A.alt "FPIndia Logo"
    case r of
      HtmlRoute_Index -> do
        renderMarkdown model "index.md"
      HtmlRoute_About -> do
        renderMarkdown model "about.md"
      HtmlRoute_UpcomingEvents -> do
        renderMarkdown model "events.md"
      HtmlRoute_PastEvents -> do
        renderMarkdown model "archive.md"
      HtmlRoute_ConnectWithUs -> do
        renderMarkdown model "connect.md"
      HtmlRoute_FpJobsInIndia -> do
        H.div ! A.class_ "my-8" $ do
          H.header "Current Jobs"
          forM_ (modelJobs model) $ \job ->
            H.li $ do
              H.b $ H.toHtml $ Jobs.jobName job
              " - "
              H.em $ H.toHtml $ Jobs.jobLocation job
        H.header "About"
        renderMarkdown model "jobs.md"
      HtmlRoute_Resources -> do
        renderMarkdown model "resources.md"
    renderFooter

renderNavbar :: Prism' FilePath Route -> H.Html
renderNavbar rp =
  H.nav ! A.class_ "w-full h-1/4 text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ universe $ \r -> renderURL (H.toHtml $ routeTitle r) (routeHref rp r)
  where
    renderURL menuItem path = H.a ! A.href path ! A.class_ "p-2 border-b-2 transition color hover:border-stone-900 duration-300" $ menuItem

renderFooter :: H.Html
renderFooter = do
  H.footer
  ! A.class_ "w-full h-10 bg-stone-600 rounded p-2 border-t-2 border-white fixed left-0 bottom-0 flex justify-center items-center text-white text-1xl"
  $ do
    H.pre "Made with " ! A.class_ "text-slate-300"
    H.a ! A.class_ "text-rose-300 hover" ! A.href "https://ema.srid.ca/" $ "Ema"

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  -- This makes the site mobile friendly by default.
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Functional Programming India"
  H.base ! A.href "/"
  -- H.script ! A.src (staticRouteUrl rp model "main.js") $ ""
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")
