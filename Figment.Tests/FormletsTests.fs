module FormletsTests

open Xunit

open System
open System.Collections.Specialized
open System.Xml.Linq
open Figment.Formlets
open Formlet

let inputInt = lift int input

let dateFormlet =
    tag "div" ["style","padding:8px"] (
        tag "span" ["style", "border: 2px solid; padding: 4px"] (
(*            puree (fun month day -> DateTime(2010, month, day)) <*>
            text "Month: " *> inputInt <*>
            text "Day: " *> inputInt
*)

            //lift2 (fun month day -> DateTime(2010, month, day)) inputInt inputInt
(*
            lift4 (fun _ month _ day -> DateTime(2010, month, day))
                (text "Month: ")
                inputInt
                (text "Day: ")
                inputInt
*)
(*
                (text "Month: " *> inputInt) ** (text "Day: " *> inputInt)
                |>> fun (month,day) -> DateTime(2010, month, day)
*)
            yields (fun month day -> DateTime(2010, month, day)) <*>
            text "Month: " *> inputInt <*>
            text "Day: " *> inputInt
            <* br <* submit "Send"
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
    let result = proc env
    printfn "%A" result
    Assert.Equal(DateTime(2010, 12, 22), (snd result).Value)

[<Fact>]
let processWithError() =
    let _, proc = run dateFormlet
    let env = NameValueCollection()
    env.Add("input_0", "aa")
    env.Add("input_1", "22")
    let proc() = proc env |> ignore
    Assert.Throws<FormatException>(Assert.ThrowsDelegate(proc)) |> ignore
    