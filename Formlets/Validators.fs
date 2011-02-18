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

    static member DefaultValidator = Validators()

    abstract member BuildValidator: ('a -> bool) -> ('a -> string) -> 'a Validator
    default x.BuildValidator a b = Formlet.err a b

    member x.IsInt =
        let isOK = Int32.TryParse >> fst
        let msg = sprintf "%s is not a valid number"
        let e = x.BuildValidator isOK msg
        satisfies e

    member x.NotEmpty = 
        let isNullOrWhiteSpace (s: string) =
            if s = null || s = ""
                then true
                else s |> Seq.exists Char.IsWhiteSpace
        let isOK = isNullOrWhiteSpace >> not
        satisfies (x.BuildValidator isOK (fun _ -> "Mandatory field"))

    member x.CreditCard =
        satisfies (x.BuildValidator luhn (fun _ -> "Invalid credit card number"))

    member x.InRange min max =
        let isOK n = n >= min && n <= max
        satisfies (x.BuildValidator isOK (fun _ -> sprintf "Field must be between %d and %d" min max))

    member x.IsEmail =
        satisfies (x.BuildValidator emailRx.IsMatch (fun _ -> "Invalid email"))

    member x.Regex pattern =
        let isOK n = Regex.IsMatch(n, pattern)
        satisfies (x.BuildValidator isOK (fun _ -> "Invalid value"))

    member x.IsUrl =
        let isOK s = Uri.TryCreate(s, UriKind.Absolute) |> fst
        satisfies (x.BuildValidator isOK (fun _ -> "Invalid URL"))
    
    member x.IsFloat =
        satisfies (x.BuildValidator (Double.TryParse >> fst) (fun _ -> "Invalid value"))

    member x.IsDecimal =
        satisfies (x.BuildValidator (Decimal.TryParse >> fst) (fun _ -> "Invalid value"))

    member x.FloatIsInt =
        satisfies (x.BuildValidator (fun (n:float) -> Math.Truncate n = n) (fun _ -> "Invalid value"))

    member x.Maxlength n =
        satisfies (x.BuildValidator (fun (s: string) -> s.Length <= n) (fun _ -> "Invalid value"))

