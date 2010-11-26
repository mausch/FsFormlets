module FormletsTests

open Xunit

open System
open System.Collections.Specialized
open System.Xml.Linq
open Figment.Formlets
open Formlet

let inputInt = puree int <*> input

let dateFormlet =
    tag "div" ["style","padding:8px"] (
        tag "span" ["style", "border: 2px solid; padding: 4px"] (
            puree (fun _ month _ day -> DateTime(2010, month, day)) <*>
            text "Month: " <*> inputInt <*>
            text "Day: " <*> inputInt
        )
    )

[<Fact>]
let renderTest() =
    let form = render "get" "/posturl" dateFormlet
    printfn "%s" (form.ToString())

[<Fact>]
let processTest() =
    let _, proc = run dateFormlet
    let env = NameValueCollection()
    env.Add("input_0", "12")
    env.Add("input_1", "22")
    let env = NameValueCollection.toList env
    let result = proc env
    printfn "%A" result
    Assert.Equal(DateTime(2010, 12, 22), result)

[<Fact>]
let processWithError() =
    let _, proc = run dateFormlet
    let env = NameValueCollection()
    env.Add("input_0", "aa")
    env.Add("input_1", "22")
    let env = NameValueCollection.toList env
    let proc() = proc env |> ignore
    Assert.Throws<FormatException>(Assert.ThrowsDelegate(proc)) |> ignore
    