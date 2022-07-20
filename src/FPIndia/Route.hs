{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module FPIndia.Route where

import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Model (Model)

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)

type StaticRoute = SR.StaticRoute "static"

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_About
  | HtmlRoute_UpcomingEvents
  | HtmlRoute_PastEvents
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
         , FileRoute "about.html"
         , FileRoute "events.html"
         , FileRoute "archive.html"
         , FileRoute "connect.html"
         , FileRoute "jobs.html"
         , FileRoute "resources.html"
         ]
     ]
    |]

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    '[ WithModel Model
     , WithSubRoutes '[HtmlRoute, StaticRoute]
     ]
    |]
