namespace Formlets

open System
open System.Drawing
open System.Text.RegularExpressions

type IValidationFunctions = 
    abstract member Int: string Formlet -> int Formlet
    abstract member Required: string Formlet -> string Formlet
    abstract member Required: bool Formlet -> bool Formlet
    abstract member CreditCard: string Formlet -> string Formlet
    abstract member LessOrEqual: 'a -> 'a Formlet -> 'a Formlet when 'a:comparison
    abstract member GreaterOrEqual: 'a -> 'a Formlet -> 'a Formlet when 'a: comparison
    abstract member InRange: 'a -> 'a -> 'a Formlet -> 'a Formlet when 'a: comparison
    abstract member Email: string Formlet -> string Formlet
    abstract member Regex : string -> (string Formlet -> string Formlet)
    abstract member Url: string Formlet -> string Formlet
    abstract member Float: string Formlet -> float Formlet
    abstract member Decimal: string Formlet -> decimal Formlet
    abstract member Maxlength: int -> string Formlet -> string Formlet
    abstract member DateTimeLocal: DateTime option -> DateTime option -> string Formlet -> DateTime Formlet
    abstract member DateTime: DateTimeOffset option -> DateTimeOffset option -> string Formlet -> DateTimeOffset Formlet
    abstract member Date: DateTime option -> DateTime option -> string Formlet -> DateTime Formlet
    abstract member Month: DateTime option -> DateTime option -> string Formlet -> DateTime Formlet
    abstract member Week: DateTime option -> DateTime option -> string Formlet -> DateTime Formlet
    abstract member Time: DateTime option -> DateTime option -> string Formlet -> DateTime Formlet
    abstract member Color: string Formlet -> Color Formlet

type IValidatorBuilder =
    abstract member Build: ('a -> bool) -> ('a -> string) -> 'a Validator

type Validate(validatorBuilder: IValidatorBuilder) as this =
    let v = this :> IValidationFunctions

    let (||.) = orF

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

    let validator isOK err =
        satisfies (validatorBuilder.Build isOK (fun _ -> err))

    let dateTime (s: _ ISerializer) min max f =
        let attr = []
        let attr = attr |> Option.mapOrId (fun v -> List.cons ("min", s.Serialize v)) min
        let attr = attr |> Option.mapOrId (fun v -> List.cons ("max", s.Serialize v)) max
        let validate = validator (s.TryDeserialize >> fst) "Invalid date"
        let f = f |> mergeAttributes attr |> validate |> map s.Deserialize
        let f = f |> Option.mapOrId (fun v -> validator ((<) v) (sprintf "Date must be after %A" v)) min
        let f = f |> Option.mapOrId (fun v -> validator ((>) v) (sprintf "Date must be after %A" v)) max
        f

    static member DefaultValidatorBuilder = 
        { new IValidatorBuilder with
            member x.Build a b = Formlet.err a b }

    static member Default = Validate(Validate.DefaultValidatorBuilder) :> IValidationFunctions

    interface IValidationFunctions with

        member x.Int f =
            let isInt = 
                let isOK = Int32.TryParse >> fst
                let msg = sprintf "%s is not a valid number"
                let e = validatorBuilder.Build isOK msg
                satisfies e
            f |> isInt |> map int

        override x.Required f =
            f 
            |> mergeAttributes ["required","required"]
            |> validator id "Required field"

        override x.Required f =
            let validate =
                let isOK = isNullOrWhiteSpace >> not
                validator isOK "Required field"

            f |> mergeAttributes ["required","required"] |> validate

        override x.CreditCard f =
            f |> validator luhn "Invalid credit card number"

        override x.LessOrEqual n f =
            let validator = validator (fun v -> v <= n) (sprintf "Value must be %A or lower" n)
            f |> mergeAttributes ["max",n.ToString()] |> validator

        override x.GreaterOrEqual n f =
            let validator = validator (fun v -> v >= n) (sprintf "Value must be %A or higher" n)
            f |> mergeAttributes ["min",n.ToString()] |> validator

        override x.InRange min max f =
            let validator = validator (fun v -> v >= min && v <= max) (sprintf "Value must be between %A and %A" min max)
            f |> mergeAttributes ["min",min.ToString(); "max",max.ToString()] |> validator

        member x.Email f =
            let isOK = String.IsNullOrEmpty ||. emailRx.IsMatch
            let validate = validator isOK "Invalid email"
            f |> mergeAttributes ["type","email"] |> validate

        member x.Regex pattern =
            let rx = Regex(pattern)
            let isOK = String.IsNullOrEmpty ||. rx.IsMatch
            fun f ->
                let validate = validator isOK "Invalid value"
                // TODO be careful with differences between .net and ecmascript regexes
                // see http://msdn.microsoft.com/en-us/library/04ses44d.aspx
                f |> mergeAttributes ["pattern",pattern] |> validate

        member x.Url f =
            let validate =
                let isOK s = Uri.TryCreate(s, UriKind.Absolute) |> fst
                validator isOK "Invalid URL"
            f |> mergeAttributes ["type","url"] |> validate

        member x.Float f =
            let validate = validator (Double.TryParse >> fst) "Invalid value"
            f |> mergeAttributes ["type","number"] |> validate |> map float

        member x.Decimal f =
            let validate = validator (Decimal.TryParse >> fst) "Invalid value"
            f |> mergeAttributes ["type","number"] |> validate |> map decimal
    
        member x.Maxlength (n: int) f =
            let validate = validator (fun (s: string) -> s.Length <= n) "Invalid value"
            f |> mergeAttributes ["maxlength",n.ToString()] |> validate

        member x.DateTimeLocal min max f =
            dateTime localDateTimeSerializer min max f

        member x.DateTime min max f =
            dateTime dateTimeSerializer min max f

        member x.Date min max f =
            dateTime dateSerializer min max f

        member x.Month min max f =
            dateTime monthSerializer min max f

        member x.Week min max f =
            dateTime weekSerializer min max f

        member x.Time min max f =
            dateTime timeSerializer min max f

        member x.Color f =
            f |> validator (colorSerializer.TryDeserialize >> fst) "Invalid color" |> map colorSerializer.Deserialize