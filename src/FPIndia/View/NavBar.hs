module FPIndia.View.NavBar (renderNavbar) where

import Prelude hiding (div, id)

import FPIndia.Model (Model)
import FPIndia.Route (HtmlRoute (..), Route)
import FPIndia.View.Util (routeHref, routeTitle, staticRouteUrl)
import Optics.Core (Prism')
import Text.Blaze.Html5 (
  AttributeValue,
  Html,
  a,
  button,
  div,
  header,
  img,
  nav,
  toHtml,
  (!),
 )
import Text.Blaze.Html5.Attributes (class_, href, id, src, type_)

renderNavbar :: Prism' FilePath Route -> Model -> HtmlRoute -> Html
renderNavbar rp model currentRoute =
  header ! class_ "flex items-center justify-between py-10" $ do
    div $
      a ! href "/" $
        div ! class_ "flex items-center justify-between" $ do
          div ! class_ "mr-3" $ do
            img ! src (staticRouteUrl rp model "fpindia-logo.png") ! class_ "h-12"
          div ! class_ "hidden h-6 text-2xl font-semibold sm:block" $ "FPIndia"
    div ! class_ "flex items-center text-base leading-5" $ do
      div ! class_ "hidden sm:block" $ do
        forM_ universe $ \r ->
          let navItem = if r == currentRoute then NavItem_Active else NavItem_Normal
           in renderNavItem rp r navItem
      div ! class_ "sm:hidden" $ do
        button ! type_ "button" ! id "burger-button" ! class_ "ml-1 mr-1 h-8 w-8 rounded py-1" $ do img ! src (staticRouteUrl rp model "burger.svg")
        div ! id "burger-menu" ! class_ "fixed top-0 left-0 z-10 h-full w-full transform bg-gray-200 opacity-95 duration-300 ease-in-out dark:bg-gray-800 translate-x-full" $ do
          nav ! class_ "fixed mt-8 h-full" $ do
            forM_ universe $ \r ->
              let navItem = if r == currentRoute then NavItem_Active else NavItem_Normal
               in renderNavItemButton rp r navItem

data NavItem = NavItem_Active | NavItem_Normal
  deriving stock (Eq, Show)

navItemClass :: NavItem -> AttributeValue
navItemClass item =
  common <> case item of
    NavItem_Active -> " border-b-2 border-stone-900"
    NavItem_Normal -> ""
  where
    common = "p-1 font-medium text-gray-900 sm:p-4"

renderNavItem :: Prism' FilePath Route -> HtmlRoute -> NavItem -> Html
renderNavItem rp r item =
  a ! href (routeHref rp r) ! class_ (navItemClass item) $ toHtml (routeTitle r)

renderNavItemButton :: Prism' FilePath Route -> HtmlRoute -> NavItem -> Html
renderNavItemButton rp r _item =
  div ! class_ "px-12 py-4 burger-menu-close" $ a ! href (routeHref rp r) ! class_ "text-2xl font-bold tracking-widest text-gray-900 dark:text-gray-100" $ toHtml (routeTitle r)
