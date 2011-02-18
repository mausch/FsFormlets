namespace Formlets

type ValidationModifiers(validators: Validators) =
    member x.Required f =
        f |> mergeAttributes ["required",""] |> validators.NotEmpty
            
    member x.Pattern pattern f =
        f |> mergeAttributes ["pattern",pattern] |> validators.Regex pattern

    member x.Float f =
        f |> mergeAttributes ["type","number"] |> validators.IsFloat |> map float

    member x.Decimal f =
        f |> mergeAttributes ["type","number"] |> validators.IsDecimal |> map decimal

    member x.Int f =
        f |> x.Float |> validators.FloatIsInt |> map int

    member x.Url f =
        f |> mergeAttributes ["type","url"] |> validators.IsUrl

    member x.Email f =
        f |> mergeAttributes ["type","email"] |> validators.IsEmail

    member x.Maxlength (n: int) f =
        f |> mergeAttributes ["maxlength",n.ToString()] |> validators.Maxlength n

    member x.LessOrEqualInt (n: int) f =
        let validator = validators.BuildValidator (fun v -> v <= n) (fun _ -> sprintf "Value must be %d or lower" n)
        f |> mergeAttributes ["max",n.ToString()] |> satisfies validator

    member x.GreaterOrEqualInt (n: int) f =
        let validator = validators.BuildValidator (fun v -> v >= n) (fun _ -> sprintf "Value must be %d or higher" n)
        f |> mergeAttributes ["min",n.ToString()] |> satisfies validator

    member x.InRangeInt (min: int) (max: int) f =
        let validator = validators.BuildValidator (fun v -> v >= min && v <= max) (fun _ -> sprintf "Value must be between %d and %d" min max)
        f |> mergeAttributes ["min",min.ToString(); "max",max.ToString()] |> satisfies validator

module Validate =
    let defaultValidator = Validators()
    let modifiers = ValidationModifiers(defaultValidator)