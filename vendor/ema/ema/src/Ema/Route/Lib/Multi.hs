{-# OPTIONS_GHC -Wno-orphans #-}

{- | Merging multiple Ema sites into one.

    This is implemented in using `sop-core`'s NS and NP types. Use as
    `MultiRoute '[MySite1, MySite2, ...]`.
-}
module Ema.Route.Lib.Multi (
  MultiRoute,
  MultiModel,
) where

import Data.SOP (I (..), NP (..), NS (..))
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism
import Ema.Site (EmaSite (..), EmaStaticSite)
import Optics.Core (equality, iso, prism', (%))

{- | The merged site's route is represented as a n-ary sum (`NS`) of the
 sub-routes.
-}
type MultiRoute (rs :: [Type]) = NS I rs

type family MultiModel (rs :: [Type]) :: [Type] where
  MultiModel '[] = '[]
  MultiModel (r ': rs) = RouteModel r : MultiModel rs

type family MultiSiteArg (rs :: [Type]) :: [Type] where
  MultiSiteArg '[] = '[]
  MultiSiteArg (r ': rs) = SiteArg r : MultiSiteArg rs

instance IsRoute (MultiRoute '[]) where
  type RouteModel (MultiRoute '[]) = NP I '[]
  routePrism = impossiblePrism
    where
      impossiblePrism :: (NP I '[] -> Prism_ FilePath (MultiRoute '[]))
      impossiblePrism Nil =
        toPrism_ $ prism' (\case {}) (const Nothing)
  routeUniverse Nil = mempty

instance
  ( IsRoute r
  , IsRoute (MultiRoute rs)
  , RouteModel (MultiRoute rs) ~ NP I (MultiModel rs)
  ) =>
  IsRoute (MultiRoute (r ': rs))
  where
  type RouteModel (MultiRoute (r ': rs)) = NP I (RouteModel r ': MultiModel rs)
  routePrism =
    routePrism @r
      `nsRoutePrism` routePrism @(MultiRoute rs)
  routeUniverse (I m :* ms) =
    fmap (toNS . Left) (routeUniverse @r m)
      <> fmap (toNS . Right) (routeUniverse @(MultiRoute rs) ms)

instance EmaSite (MultiRoute '[]) where
  type SiteArg (MultiRoute '[]) = NP I '[]
  siteInput _ Nil = pure $ pure Nil
  siteOutput _ Nil = \case {}

instance
  ( EmaStaticSite r
  , EmaStaticSite (MultiRoute rs)
  , SiteArg (MultiRoute rs) ~ NP I (MultiSiteArg rs)
  , RouteModel (MultiRoute rs) ~ NP I (MultiModel rs)
  ) =>
  EmaSite (MultiRoute (r ': rs))
  where
  type SiteArg (MultiRoute (r ': rs)) = NP I (MultiSiteArg (r ': rs))
  siteInput cliAct (I i :* is) = do
    m <- siteInput @r cliAct i
    ms <- siteInput @(MultiRoute rs) cliAct is
    pure $ curry toNP <$> m <*> ms
  siteOutput rp (I m :* ms) =
    fromNS
      >>> either
        (siteOutput @r (rp % headRoute) m)
        (siteOutput @(MultiRoute rs) (rp % tailRoute) ms)
    where
      tailRoute =
        (prism' (toNS . Right) (fromNS >>> rightToMaybe))
      headRoute =
        (prism' (toNS . Left) (fromNS >>> leftToMaybe))

-- | Like `eitherRoutePrism` but uses sop-core types instead of Either/Product.
nsRoutePrism ::
  (a -> Prism_ FilePath r) ->
  (NP I as -> Prism_ FilePath (NS I rs)) ->
  (NP I (a ': as) -> Prism_ FilePath (NS I (r ': rs)))
nsRoutePrism a b =
  eitherRoutePrism a b
    & mapRoutePrism equality (iso toNS fromNS) fromNP

fromNP :: NP I (a ': as) -> (a, NP I as)
fromNP (I x :* y) = (x, y)

toNP :: (a, NP I as) -> NP I (a ': as)
toNP (x, y) = I x :* y

fromNS :: NS I (a ': as) -> Either a (NS I as)
fromNS = \case
  Z (I x) -> Left x
  S xs -> Right xs

toNS :: Either a (NS I as) -> NS I (a ': as)
toNS = either (Z . I) S
