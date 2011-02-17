namespace Formlets

open System

module Validators = 
    // from http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#F.23
    let luhn (s:string) =
        let rec g r c = function
        | 0 -> r
        | i ->
            let d = ((int s.[i - 1]) - 48) <<< c
            g (r + if d < 10 then d else d - 9) (1 - c) (i - 1)
        (g 0 0 s.Length) % 10 = 0

    open System.Text.RegularExpressions
    let private rx = // from http://haacked.com/archive/2007/08/21/i-knew-how-to-validate-an-email-address-until-i.aspx
        Regex(@"^(?!\.)(""([^""\r\\]|\\[""\r\\])*""|" 
            + @"([-a-z0-9!#$%&'*+/=?^_`{|}~]|(?<!\.)\.)*)(?<!\.)" 
            + @"@[a-z0-9][\w\.-]*[a-z0-9]\.[a-z][a-z\.]*[a-z]$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let email (s: string) = rx.IsMatch s

    let url (s: string) = Uri.TryCreate(s, UriKind.Absolute) |> fst

module Validate =
    let isInt =
        let isOK = Int32.TryParse >> fst
        let msg = sprintf "%s is not a valid number"
        let e = err isOK msg
        satisfies e

    let notEmpty =
        let isNullOrWhiteSpace (s: string) =
            if s = null || s = ""
                then true
                else s |> Seq.exists Char.IsWhiteSpace
        let isOK = isNullOrWhiteSpace >> not
        satisfies (err isOK (fun _ -> "Mandatory field"))

    let creditCard =
        satisfies (err Validators.luhn (fun _ -> "Invalid credit card number"))

    let inRange min max =
        let isOK n = n >= min && n <= max
        satisfies (err isOK (fun _ -> sprintf "Field must be between %d and %d" min max))

    let isEmail =
        satisfies (err Validators.email (fun _ -> "Invalid email"))

    open System.Text.RegularExpressions
    let regex pattern =
        let isOK n = Regex.IsMatch(n, pattern)
        satisfies (err isOK (fun _ -> "Invalid value"))

    let isUrl =
        satisfies (err Validators.url (fun _ -> "Invalid URL"))