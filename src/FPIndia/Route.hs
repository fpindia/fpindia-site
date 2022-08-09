{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module FPIndia.Route where

import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Model (Model)

type StaticRoute = SR.StaticRoute "static"

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_Events
  | HtmlRoute_ConnectWithUs
  | HtmlRoute_FpJobsInIndia
  | HtmlRoute_Resources
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

deriveGeneric ''HtmlRoute
deriveIsRoute
  ''HtmlRoute
  [t|
    '[ WithSubRoutes
        '[ FileRoute "index.html"
         , FileRoute "events.html"
         , FileRoute "connect.html"
         , FileRoute "jobs.html"
         , FileRoute "resources.html"
         ]
     ]
    |]

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    '[ WithModel Model
     , WithSubRoutes '[HtmlRoute, StaticRoute]
     ]
    |]
