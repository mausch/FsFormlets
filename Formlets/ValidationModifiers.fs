namespace Formlets

type ValidationModifiers(validators: Validators) =
    member x.Required f =
        f |> mergeAttributes ["required",""] |> validators.notEmpty
            
    member x.Pattern pattern f =
        f |> mergeAttributes ["pattern",pattern] |> validators.regex pattern

    member x.Float f =
        f |> mergeAttributes ["type","number"] |> validators.isFloat |> map float

    member x.Decimal f =
        f |> mergeAttributes ["type","number"] |> validators.isDecimal |> map decimal

    member x.Int f =
        f |> x.Float |> validators.floatIsInt |> map int

    member x.Url f =
        f |> mergeAttributes ["type","url"] |> validators.isUrl

    member x.Email f =
        f |> mergeAttributes ["type","email"] |> validators.isEmail

    member x.Maxlength (n: int) f =
        f |> mergeAttributes ["maxlength",n.ToString()] |> validators.maxlength n

    member x.LessOrEqualInt (n: int) f =
        let validator = validators.BuildValidator (fun v -> v <= n) (fun _ -> sprintf "Value must be %d or lower" n)
        f |> satisfies validator

    member x.GreaterOrEqualInt (n: int) f =
        let validator = validators.BuildValidator (fun v -> v >= n) (fun _ -> sprintf "Value must be %d or higher" n)
        f |> satisfies validator

    member x.InRangeInt (min: int) (max: int) f =
        let validator = validators.BuildValidator (fun v -> v >= min && v <= max) (fun _ -> sprintf "Value must be between %d and %d" min max)
        f |> satisfies validator

module Validate =
    let defaultValidator = Validators()
    let modifiers = ValidationModifiers(defaultValidator)