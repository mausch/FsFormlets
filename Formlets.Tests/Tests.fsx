#r @"..\packages\Fuchu.0.1.3\lib\net40-client\Fuchu.dll"
#r @"..\packages\FSharpx.Core.1.6.4\lib\40\Fsharpx.Core.dll"
#r "System.Xml.Linq"
#r "System.Web.Abstractions"
#r @"..\Formlets\bin\Debug\Formlets.dll"

#load "prelude.fs"
#load @"FormletsTests.fs"

open Fuchu

Formlets.Tests.Formlets.tests
|> Test.filter (fun s -> s.Contains "Applicatives")
|> run