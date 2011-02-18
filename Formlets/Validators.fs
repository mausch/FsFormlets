namespace Formlets

open System
open System.Text.RegularExpressions

type Validators() =
    // from http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#F.23
    let luhn (s:string) =
        let rec g r c = function
        | 0 -> r
        | i ->
            let d = ((int s.[i - 1]) - 48) <<< c
            g (r + if d < 10 then d else d - 9) (1 - c) (i - 1)
        (g 0 0 s.Length) % 10 = 0

    let emailRx = // from http://haacked.com/archive/2007/08/21/i-knew-how-to-validate-an-email-address-until-i.aspx
        Regex(@"^(?!\.)(""([^""\r\\]|\\[""\r\\])*""|" 
            + @"([-a-z0-9!#$%&'*+/=?^_`{|}~]|(?<!\.)\.)*)(?<!\.)" 
            + @"@[a-z0-9][\w\.-]*[a-z0-9]\.[a-z][a-z\.]*[a-z]$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    let email (s: string) = emailRx.IsMatch s

    let url (s: string) = Uri.TryCreate(s, UriKind.Absolute) |> fst

    static member DefaultValidator = Validators()

    abstract member BuildValidator: ('a -> bool) -> ('a -> string) -> 'a Validator
    default x.BuildValidator a b = Formlet.err a b

    member x.isInt =
        let isOK = Int32.TryParse >> fst
        let msg = sprintf "%s is not a valid number"
        let e = x.BuildValidator isOK msg
        satisfies e

    member x.notEmpty = 
        let isNullOrWhiteSpace (s: string) =
            if s = null || s = ""
                then true
                else s |> Seq.exists Char.IsWhiteSpace
        let isOK = isNullOrWhiteSpace >> not
        satisfies (x.BuildValidator isOK (fun _ -> "Mandatory field"))

    member x.creditCard =
        satisfies (x.BuildValidator luhn (fun _ -> "Invalid credit card number"))

    member x.inRange min max =
        let isOK n = n >= min && n <= max
        satisfies (x.BuildValidator isOK (fun _ -> sprintf "Field must be between %d and %d" min max))

    member x.isEmail =
        satisfies (x.BuildValidator email (fun _ -> "Invalid email"))

    member x.regex pattern =
        let isOK n = Regex.IsMatch(n, pattern)
        satisfies (x.BuildValidator isOK (fun _ -> "Invalid value"))

    member x.isUrl =
        satisfies (x.BuildValidator url (fun _ -> "Invalid URL"))

module Validate =
    let defaultValidator = Validators()