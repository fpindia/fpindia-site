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
      H.body ! A.class_ "text-stone-900 antialiased" $ do
        renderBody rp m r

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "mx-auto max-w-3xl px-4" $ do
    H.div ! A.class_ "container mx-auto flex flex-col mt-8 p-2 pb-0" $ do
      NavBar.renderNavbar rp model r
      H.main $ case r of
        HtmlRoute_Index -> do
          renderMarkdown model "index.md"
        HtmlRoute_Events -> do
          renderMarkdown model "events.md"
        HtmlRoute_ConnectWithUs -> do
          renderMarkdown model "connect.md"
        HtmlRoute_FpJobsInIndia -> do
          H.div ! A.class_ "my-4" $ do
            H.img ! A.class_ "h-20 m-auto mb-5" ! A.src (staticRouteUrl rp model "underconstruction.svg")
            H.header ! A.class_ "text-2xl font-bold pb-6" $ "Current Jobs"
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
          renderMarkdown model "jobs.md"
        HtmlRoute_Resources -> do
          renderMarkdown model "resources.md"
      renderFooter rp model

renderFooter :: Prism' FilePath Route -> Model -> H.Html
renderFooter rp model = do
  H.footer $
    H.div ! A.class_ "mt-16 flex flex-col items-center" $ do
      H.div ! A.class_ "mb-3 flex space-x-4" $ do
        footerLink "mail.svg" "Mail" "mailto:team@functionalprogramming.in"
        footerLink "github.svg" "Github" "https://github.com/fpindia/"
        footerLink "youtube.svg" "Youtube" "https://www.youtube.com/channel/UCiySROube0vutFBu0M7pvxg"
        footerLink "twitter.svg" "Youtube" "https://twitter.com/functionalindia"
        footerLink "telegram.svg" "Telegram" "https://t.me/fpncr"
        footerLink "discord.svg" "Discord" "https://discord.gg/sDHfscRdh7"
      H.div ! A.class_ "mb-2 flex space-x-2 text-sm text-gray-500 dark:text-gray-400" $ do
        H.div "Functional Programming India © 2022"
      H.div ! A.class_ "mb-8 text-sm text-gray-500 dark:text-gray-400" $ do
        "Made with "
        H.a ! A.class_ "text-rose-700" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.href "https://www.haskell.org/" $ "Haskell"
        " and "
        H.a ! A.class_ "text-rose-700" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.href "https://ema.srid.ca/" $ "Ema"
  where
    footerLink image altText url =
      H.a ! A.class_ "text-sm text-gray-500 transition h-8 v-8" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.href url $ do
        H.span ! A.class_ "sr-only" $ altText
        H.img ! A.class_ "fill-current text-gray-700 hover:border-b-2 hover:text-blue-500 dark:text-gray-200 dark:hover:text-blue-400 h-6 w-6" ! A.src (staticRouteUrl rp model image)

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  -- This makes the site mobile friendly by default.
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Functional Programming India"
  H.base ! A.href "/"
  H.script ! A.src (staticRouteUrl rp model "main.js") $ ""
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")
