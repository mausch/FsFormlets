namespace SampleApp

open System
open System.Web
open System.Web.Mvc
open System.Web.Routing

type MvcApplication() =
    inherit HttpApplication()

    member this.RegisterRoutes(routes: RouteCollection) = 
        routes.IgnoreRoute "asd"

    member this.Application_Start() =
        this.RegisterRoutes RouteTable.Routes
    

