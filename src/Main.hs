{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Ema
import Ema.Route.Generic
import FPIndia.Common (tailwindLayout)
import Generics.SOP qualified as SOP
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Route_Index
  | Route_About
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes)
    via ( Route
            `WithSubRoutes` [ FileRoute "index.html"
                            , FileRoute "about.html"
                            ]
        )
  deriving
    (HasSubModels, IsRoute)
    via (Route `WithModel` ())

instance EmaSite Route where
  siteInput _ _ = pure $ pure ()
  siteOutput rp () r =
    Ema.AssetGenerated Ema.Html $
      tailwindLayout (H.title "FPIndia" >> H.base ! A.href "/") $
        H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
          H.h1 ! A.class_ "text-3xl font-bold" $ "FPIndia WIP"
          case r of
            Route_Index -> do
              "You are on the index page. "
              routeElem Route_About "Go to About"
            Route_About -> do
              routeElem Route_Index "Go to Index"
              ". You are on the about page. "
    where
      routeElem r' w = do
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl rp r')

main :: IO ()
main = void $ Ema.runSite @Route ()
