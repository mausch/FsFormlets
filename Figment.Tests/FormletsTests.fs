module FormletsTests

open Xunit

open System
open System.Collections.Specialized
open System.Xml.Linq
open Figment.Formlets
open Formlet

let isInt v = Int32.TryParse v |> fst

let intValidator : string Validator =
    err isInt (sprintf "%s is not a valid number")

let inputInt = lift int (input |> satisfies intValidator)

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
    let xml, proc = run dateFormlet
    let env = NameValueCollection()
    env.Add("input_0", "aa")
    env.Add("input_1", "22")
    let err,value = proc env
    let xdoc = XmlWriter.render "" "" err
    printfn "Error form:\n%s" (xdoc.ToString())
    Assert.True(value.IsNone)
    