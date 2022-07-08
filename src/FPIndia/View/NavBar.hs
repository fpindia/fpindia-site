module FPIndia.View.NavBar where

import FPIndia.Route (HtmlRoute (..), Route)
import FPIndia.View.Util (routeHref, routeTitle)
import Optics.Core (Prism')
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

renderNavbar :: Prism' FilePath Route -> HtmlRoute -> H.Html
renderNavbar rp currentRoute =
  H.nav ! A.class_ "w-full h-1/4 text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ universe $ \route ->
      if route == currentRoute
        then renderURL route True
        else renderURL route False
  where
    renderURL r isActive =
      let style =
            ( if isActive
                then "p-2 border-b-2 border-stone-900"
                else "p-2 border-b-2 transition color hover:border-stone-900 duration-300"
            )
       in H.a ! A.href (routeHref rp r) ! A.class_ style $ H.toHtml (routeTitle r)
