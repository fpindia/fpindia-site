module FPIndia.StaticRoute where

import Data.Map.Strict qualified as Map
import Ema
import Optics.Core (prism')

-- | Route to a static file under `./static`.
newtype StaticRoute (a :: Type) = StaticRoute {unStaticRoute :: FilePath}
  deriving newtype (Eq, Ord, Show)

-- A route encoder is simply a Prism with (some) model as its context.
-- `mkRouteEncoder` enables you to create route encoders "manually" this way.
instance IsRoute (StaticRoute a) where
  type RouteModel (StaticRoute a) = Map FilePath a
  routeEncoder = mkRouteEncoder $ \files ->
    let enc =
          unStaticRoute
        dec fp =
          StaticRoute fp <$ guard (Map.member fp files)
     in prism' enc dec
  allRoutes files =
    StaticRoute <$> Map.keys files
