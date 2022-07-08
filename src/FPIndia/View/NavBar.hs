module FPIndia.View.NavBar (renderNavbar) where

import FPIndia.Route (HtmlRoute (..), Route)
import FPIndia.View.Util (routeHref, routeTitle)
import Optics.Core (Prism')
import Text.Blaze.Html5 (AttributeValue, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

renderNavbar :: Prism' FilePath Route -> HtmlRoute -> H.Html
renderNavbar rp currentRoute =
  H.nav ! A.class_ "w-full h-1/4 text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ universe $ \r ->
      let navItem = if r == currentRoute then NavItem_Active else NavItem_Normal
       in renderNavItem rp r navItem

data NavItem = NavItem_Active | NavItem_Normal
  deriving stock (Eq, Show)

navItemClass :: NavItem -> AttributeValue
navItemClass = \case
  NavItem_Active -> "border-b-2 border-stone-900"
  NavItem_Normal -> "p-2 border-b-2 transition color hover:border-stone-900 duration-300"

renderNavItem :: Prism' FilePath Route -> HtmlRoute -> NavItem -> H.Html
renderNavItem rp r item =
  H.a ! A.href (routeHref rp r) ! A.class_ (navItemClass item) $ H.toHtml (routeTitle r)
