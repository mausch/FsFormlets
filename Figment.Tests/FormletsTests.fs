module FormletsTests

open Xunit

open System
open System.Xml.Linq
open Figment.Formlets
open Formlet

let inputInt = puree int <*> input

let dateFormlet =
    tag "div" ["style","padding:8px"] (
        tag "span" ["style", "border: 2px solid; padding: 4px"] (
            puree (fun _ month _ day -> new DateTime(2010, month, day)) <*>
            text "Month: " <*> inputInt <*>
            text "Day: " <*> inputInt
        )
    )

[<Fact>]
let renderTest() =
    let form = render dateFormlet
    printfn "%s" (form.ToString())
