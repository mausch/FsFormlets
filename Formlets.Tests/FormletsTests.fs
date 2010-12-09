module FormletsTests

open Xunit

open System
open System.Collections.Specialized
open System.Globalization
open System.Xml.Linq
open Formlets

let isInt = Int32.TryParse >> fst

let intValidator : string Validator =
    err isInt (sprintf "%s is not a valid number")

let inputInt = lift int (input |> satisfies intValidator)
//let inputInt = yields (fun i -> int i) <*> (input |> satisfies intValidator) // equivalent to above

let dateFormlet =
    let baseFormlet = 
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
(*
            yields (fun month day -> DateTime(2010, month, day)) <*>
            text "Month: " *> inputInt <*>
            text "Day: " *> inputInt
            <* br <* submit "Send" 
*)

                yields (fun month day -> month,day) <*>
                text "Month: " *> inputInt <*>
                text "Day: " *> inputInt
                <* br <* submit "Send" 
            )
        )
    let isDate (month,day) = 
        DateTime.TryParseExact(sprintf "%d%d%d" 2010 month day, "yyyyMMdd", CultureInfo.InvariantCulture, DateTimeStyles.None) |> fst
    let dateValidator = err isDate (fun (month,day) -> sprintf "%d/%d is not a valid date" month day)
    let validatingFormlet = baseFormlet |> satisfies dateValidator
    lift (fun (month,day) -> DateTime(2010, month, day)) validatingFormlet


[<Fact>]
let renderTest() =
    let form = render dateFormlet
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
let processWithInvalidInt() =
    let xml, proc = run dateFormlet
    let env = NameValueCollection()
    env.Add("input_0", "aa")
    env.Add("input_1", "22")
    let err,value = proc env
    let xdoc = XmlWriter.render [Tag("div", [], xml @ err)]
    printfn "Error form:\n%s" (xdoc.ToString())
    Assert.True(value.IsNone)

[<Fact>]
let processWithInvalidInts() =
    let xml, proc = run dateFormlet
    let env = NameValueCollection()
    env.Add("input_0", "aa")
    env.Add("input_1", "bb")
    let err,value = proc env
    let xdoc = XmlWriter.render [Tag("div", [], xml @ err)]
    printfn "Error form:\n%s" (xdoc.ToString())
    Assert.True(value.IsNone)

[<Fact>]
let processWithInvalidDate() =
    let xml, proc = run dateFormlet
    let env = NameValueCollection()
    env.Add("input_0", "22")
    env.Add("input_1", "22")
    let err,value = proc env
    let xdoc = XmlWriter.render [Tag("div", [], xml @ err)]
    printfn "Error form:\n%s" (xdoc.ToString())
    Assert.True(value.IsNone)
    