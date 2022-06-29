{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Generics.Sum.Any (AsAny (_As))
import Data.SOP (I (I), NP (Nil, (:*)))
import Data.Time (UTCTime)
import Ema
import Ema.Route.Generic
import FPIndia.Common (tailwindLayout)
import FPIndia.StaticRoute (StaticRoute)
import Generics.SOP qualified as SOP
import Optics.Core (Prism', (%))
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
  { modelFiles :: Map FilePath UTCTime
  }
  deriving stock (Eq, Show)

emptyModel :: Model
emptyModel = Model mempty

instance EmaSite Route where
  siteInput _ _ = pure $ pure emptyModel
  siteOutput rp m = \case
    Route_Html r ->
      Ema.AssetGenerated Ema.Html $ renderHtmlRoute (rp % (_As @"Route_Html")) r

renderHtmlRoute :: Prism' FilePath HtmlRoute -> HtmlRoute -> LByteString
renderHtmlRoute rp r = do
  tailwindLayout (H.title "FPIndia" >> H.base ! A.href "/") $
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
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl rp r')

main :: IO ()
main = void $ Ema.runSite @Route ()
