{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module FPIndia.Route where

import Ema
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import FPIndia.Model (Model)
import Generics.SOP qualified as SOP

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , WithSubRoutes '[HtmlRoute, StaticRoute]
             ]
        )

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
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            HtmlRoute
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
        )
