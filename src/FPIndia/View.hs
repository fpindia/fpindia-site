{-# LANGUAGE UndecidableInstances #-}

module FPIndia.View where

import FPIndia.Jobs qualified as Jobs
import FPIndia.Model (Model (modelJobs))
import FPIndia.Route (HtmlRoute (..), Route)
import FPIndia.View.NavBar qualified as NavBar
import FPIndia.View.Util (renderMarkdown, routeTitle, staticRouteUrl)
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
      H.body ! A.class_ "overflow-y-scroll text-stone-900" $ do
        renderBody rp m r

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "container mx-auto flex flex-col items-center justify-center mt-8 p-2 mb-10" $ do
    NavBar.renderNavbar rp r
    H.h1 ! A.class_ "text-3xl font-bold" $ H.toHtml $ routeTitle r
    case r of
      HtmlRoute_Index -> do
        renderMarkdown model "index.md"
        H.img ! A.src (staticRouteUrl rp model "fpindia-logo.png") ! A.class_ "w-32" ! A.alt "FPIndia Logo"
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
          H.table ! A.class_ "table table-auto border-2 border-y-black container text-center max-w-3xl" $ do
            H.thead $ do
              H.tr $ do
                H.th "Job name"
                H.th "Website"
                H.th "Source"
                H.th "Location"
                H.th "Languages"
                H.th "Permalink"
                H.th "Active Status"
            forM_ (modelJobs model) $ \job ->
              H.tbody $ do
                H.tr $ do
                  H.td $ H.toHtml $ Jobs.jobName job
                  H.td $ H.toHtml $ Jobs.jobWebsite job
                  H.td $ H.toHtml $ Jobs.jobSource job
                  H.td $ H.toHtml $ Jobs.jobLocation job
                  H.td $ H.toHtml $ Jobs.jobLanguages job
                  H.td $ H.toHtml $ Jobs.jobPermalink job
                  H.td $ H.toHtml $ Jobs.jobActiveStatus job
        H.header "About"
        renderMarkdown model "jobs.md"
      HtmlRoute_Resources -> do
        renderMarkdown model "resources.md"
    renderFooter

renderFooter :: H.Html
renderFooter = do
  H.footer
  ! A.class_ "flex flex-col items-center justify-center  bg-gray-50 rounded p-2 border-t-2 border-white text-sm"
  $ do
    H.div $ do
      "Made with "
      H.a ! A.class_ "text-rose-700 font-bold" ! A.target "_blank" ! A.href "https://ema.srid.ca/" $ "Ema"

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  -- This makes the site mobile friendly by default.
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Functional Programming India"
  H.base ! A.href "/"
  -- H.script ! A.src (staticRouteUrl rp model "main.js") $ ""
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")
