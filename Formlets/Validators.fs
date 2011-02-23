namespace Formlets

open System
open System.Text.RegularExpressions

type IValidate = 
    abstract member BuildValidator: ('a -> bool) -> ('a -> string) -> 'a Validator    
    abstract member Int: string Formlet -> int Formlet
    abstract member Required: string Formlet -> string Formlet
    abstract member Required: bool Formlet -> bool Formlet
    abstract member CreditCard: string Formlet -> string Formlet
    abstract member LessOrEqual: ('a -> string) -> 'a -> 'a Formlet -> 'a Formlet when 'a:comparison
    abstract member LessOrEqualInt: int -> int Formlet -> int Formlet
    abstract member LessOrEqualFloat: float -> float Formlet -> float Formlet
    abstract member GreaterOrEqual: ('a -> string) -> 'a -> 'a Formlet -> 'a Formlet when 'a: comparison
    abstract member GreaterOrEqualInt: int -> int Formlet -> int Formlet
    abstract member GreaterOrEqualFloat: float -> float Formlet -> float Formlet
    abstract member InRange: ('a -> 'a -> string) -> 'a -> 'a -> 'a Formlet -> 'a Formlet when 'a: comparison
    abstract member InRangeInt: int -> int -> int Formlet -> int Formlet
    abstract member InRangeFloat: float -> float -> float Formlet -> float Formlet
    abstract member Email: string Formlet -> string Formlet
    abstract member Regex : string -> string Formlet -> string Formlet
    abstract member Url: string Formlet -> string Formlet
    abstract member Float: string Formlet -> float Formlet
    abstract member Decimal: string Formlet -> decimal Formlet
    abstract member Maxlength: int -> string Formlet -> string Formlet

type Validate() as this =
    let v = this :> IValidate

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

    static member Default = Validate() :> IValidate

    interface IValidate with
        member x.BuildValidator a b = Formlet.err a b

        member x.Int f =
            let isInt = 
                let isOK = Int32.TryParse >> fst
                let msg = sprintf "%s is not a valid number"
                let e = v.BuildValidator isOK msg
                satisfies e
            f |> isInt |> map int

        override x.Required f =
            f 
            |> mergeAttributes ["required","required"]
            |> satisfies (v.BuildValidator ((=)true) (fun _ -> "Required field"))

        override x.Required f =
            let validate =
                let isNullOrWhiteSpace (s: string) =
                    if s = null || s = ""
                        then true
                        else s |> Seq.exists Char.IsWhiteSpace
                let isOK = isNullOrWhiteSpace >> not
                satisfies (v.BuildValidator isOK (fun _ -> "Required field"))

            f |> mergeAttributes ["required","required"] |> validate

        override x.CreditCard f =
            f |> satisfies (v.BuildValidator luhn (fun _ -> "Invalid credit card number"))

        override x.LessOrEqual printer n f =
            let validator = v.BuildValidator (fun v -> v <= n) (fun _ -> printer n)
            f |> mergeAttributes ["max",n.ToString()] |> satisfies validator

        override x.LessOrEqualInt n f =
            v.LessOrEqual (sprintf "Value must be %d or lower") n f

        override x.LessOrEqualFloat n f =
            v.LessOrEqual (sprintf "Value must be %f or lower") n f

        override x.GreaterOrEqual printer n f =
            let validator = v.BuildValidator (fun v -> v >= n) (fun _ -> printer n)
            f |> mergeAttributes ["min",n.ToString()] |> satisfies validator

        override x.GreaterOrEqualInt n f =
            v.GreaterOrEqual (sprintf "Value must be %d or higher") n f

        override x.GreaterOrEqualFloat n f =
            v.GreaterOrEqual (sprintf "Value must be %f or higher") n f

        override x.InRange printer min max f =
            let validator = v.BuildValidator (fun v -> v >= min && v <= max) (fun _ -> printer min max)
            f |> mergeAttributes ["min",min.ToString(); "max",max.ToString()] |> satisfies validator

        override x.InRangeInt min max f =
            v.InRange (sprintf "Value must be between %d and %d") min max f

        member x.InRangeFloat min max f = 
            v.InRange (sprintf "Value must be between %f and %f") min max f

        member x.Email f =
            let validate = 
                satisfies (v.BuildValidator emailRx.IsMatch (fun _ -> "Invalid email"))
            f |> mergeAttributes ["type","email"] |> validate

        member x.Regex pattern f =
            let validate = 
                let isOK n = Regex.IsMatch(n, pattern)
                satisfies (v.BuildValidator isOK (fun _ -> "Invalid value"))
            f |> mergeAttributes ["pattern",pattern] |> validate

        member x.Url f =
            let validate =
                let isOK s = Uri.TryCreate(s, UriKind.Absolute) |> fst
                satisfies (v.BuildValidator isOK (fun _ -> "Invalid URL"))
            f |> mergeAttributes ["type","url"] |> validate

        member x.Float f =
            let validate = 
                satisfies (v.BuildValidator (Double.TryParse >> fst) (fun _ -> "Invalid value"))
            f |> mergeAttributes ["type","number"] |> validate |> map float

        member x.Decimal f =
            let validate =
                satisfies (v.BuildValidator (Decimal.TryParse >> fst) (fun _ -> "Invalid value"))
            f |> mergeAttributes ["type","number"] |> validate |> map decimal
    
        member x.Maxlength (n: int) f =
            let validate =
                satisfies (v.BuildValidator (fun (s: string) -> s.Length <= n) (fun _ -> "Invalid value"))
            f |> mergeAttributes ["maxlength",n.ToString()] |> validate
