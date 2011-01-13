namespace Formlets

open System

module Validate =
    let isInt =
        let isOK = Int32.TryParse >> fst
        let msg = sprintf "%s is not a valid number"
        let e = err isOK msg
        satisfies e


