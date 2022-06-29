-- | TODO: Upstream this to Ema.
module FPIndia.StaticRoute where

import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)
import Ema
import Optics.Core (prism')
import System.UnionMount qualified as UnionMount
import UnliftIO (MonadUnliftIO)

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

staticFilesDynamic ::
  forall m.
  (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m) =>
  FilePath ->
  m (Dynamic m (Map FilePath UTCTime))
staticFilesDynamic baseDir = do
  let pats = [((), "**")]
      ignorePats = [".*"]
      model0 = mempty
  Dynamic <$> UnionMount.mount baseDir pats ignorePats model0 (const handleUpdate)
  where
    handleUpdate ::
      FilePath ->
      UnionMount.FileAction () ->
      m (Map FilePath UTCTime -> Map FilePath UTCTime)
    handleUpdate fp = \case
      UnionMount.Refresh _ _ -> do
        lastAccessed <- liftIO getCurrentTime
        pure $ Map.insert fp lastAccessed
      UnionMount.Delete -> do
        pure $ Map.delete fp
