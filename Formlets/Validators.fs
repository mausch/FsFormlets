namespace Formlets

open System
open System.Text.RegularExpressions

type Validate() =
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

    static member Default = Validate()

    abstract member BuildValidator: ('a -> bool) -> ('a -> string) -> 'a Validator
    default x.BuildValidator a b = Formlet.err a b

    member x.Int f =
        let isInt = 
            let isOK = Int32.TryParse >> fst
            let msg = sprintf "%s is not a valid number"
            let e = x.BuildValidator isOK msg
            satisfies e
        f |> isInt |> map int

    member x.Required f =
        let validate =
            let isNullOrWhiteSpace (s: string) =
                if s = null || s = ""
                    then true
                    else s |> Seq.exists Char.IsWhiteSpace
            let isOK = isNullOrWhiteSpace >> not
            satisfies (x.BuildValidator isOK (fun _ -> "Required field"))

        f |> mergeAttributes ["required","required"] |> validate

    member x.CreditCard =
        satisfies (x.BuildValidator luhn (fun _ -> "Invalid credit card number"))

    member x.LessOrEqual printer n f =
        let validator = x.BuildValidator (fun v -> v <= n) (fun _ -> printer n)
        f |> mergeAttributes ["max",n.ToString()] |> satisfies validator

    member x.LessOrEqualInt =
        x.LessOrEqual (sprintf "Value must be %d or lower")

    member x.LessOrEqualFloat =
        x.LessOrEqual (sprintf "Value must be %f or lower")

    member x.GreaterOrEqual printer n f =
        let validator = x.BuildValidator (fun v -> v >= n) (fun _ -> printer n)
        f |> mergeAttributes ["min",n.ToString()] |> satisfies validator

    member x.GreaterOrEqualInt =
        x.GreaterOrEqual (sprintf "Value must be %d or higher")

    member x.GreaterOrEqualFloat =
        x.GreaterOrEqual (sprintf "Value must be %f or higher")

    member x.InRange printer min max f =
        let validator = x.BuildValidator (fun v -> v >= min && v <= max) (fun _ -> printer min max)
        f |> mergeAttributes ["min",min.ToString(); "max",max.ToString()] |> satisfies validator

    member x.InRangeInt =
        x.InRange (sprintf "Value must be between %d and %d")

    member x.InRangeFloat = 
        x.InRange (sprintf "Value must be between %f and %f")

    member x.Email f =
        let validate = 
            satisfies (x.BuildValidator emailRx.IsMatch (fun _ -> "Invalid email"))
        f |> mergeAttributes ["type","email"] |> validate

    member x.Regex pattern f =
        let validate = 
            let isOK n = Regex.IsMatch(n, pattern)
            satisfies (x.BuildValidator isOK (fun _ -> "Invalid value"))
        f |> mergeAttributes ["pattern",pattern] |> validate

    member x.Url f =
        let validate =
            let isOK s = Uri.TryCreate(s, UriKind.Absolute) |> fst
            satisfies (x.BuildValidator isOK (fun _ -> "Invalid URL"))
        f |> mergeAttributes ["type","url"] |> validate

    member x.Float f =
        let validate = 
            satisfies (x.BuildValidator (Double.TryParse >> fst) (fun _ -> "Invalid value"))
        f |> mergeAttributes ["type","number"] |> validate |> map float

    member x.Decimal f =
        let validate =
            satisfies (x.BuildValidator (Decimal.TryParse >> fst) (fun _ -> "Invalid value"))
        f |> mergeAttributes ["type","number"] |> validate |> map decimal
    
    member x.FloatIsInt =
        satisfies (x.BuildValidator (fun (n:float) -> Math.Truncate n = n) (fun _ -> "Invalid value"))

    member x.Maxlength (n: int) f =
        let validate =
            satisfies (x.BuildValidator (fun (s: string) -> s.Length <= n) (fun _ -> "Invalid value"))
        f |> mergeAttributes ["maxlength",n.ToString()] |> validate
