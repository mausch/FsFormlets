namespace Formlets

open System

module Validate =
    let isInt =
        let isOK = Int32.TryParse >> fst
        let msg = sprintf "%s is not a valid number"
        let e = err isOK msg
        satisfies e

    let notEmpty =
        let isOK = String.IsNullOrWhiteSpace >> not
        satisfies (err isOK (fun _ -> "Mandatory field"))

    let creditCard =
        // from http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#F.23
        let luhn (s:string) =
            let rec g r c = function
            | 0 -> r
            | i ->
                let d = ((int s.[i - 1]) - 48) <<< c
                g (r + if d < 10 then d else d - 9) (1 - c) (i - 1)
            (g 0 0 s.Length) % 10 = 0
        satisfies (err luhn (fun _ -> "Invalid credit card number"))