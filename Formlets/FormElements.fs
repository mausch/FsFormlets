namespace Formlets

open System

/// <summary>
/// Validated form elements.
/// These generate the appropriate HTML5 element so you can also use HTML5 for client-side validation
/// </summary>
type FormElements(validators: IValidate) =
    member x.Validate = validators

    member x.Checkbox(value, ?required, ?attributes) =
        let attributes = defaultArg attributes []
        let formlet = Formlet.checkbox value attributes
        formlet |> Option.mapBoolOrId validators.Required required
   
    member x.Textarea(?value, ?attributes: (string * string) list) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        Formlet.textarea value attributes

    member private x.iText(value: string option, attributes: (string*string) list option, required: bool option, maxlength: int option, pattern: string option) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        let formlet = Formlet.input value attributes
        let formlet = formlet |> Option.mapBoolOrId validators.Required required
        let formlet = formlet |> Option.mapOrId validators.Maxlength maxlength
        formlet |> Option.mapOrId validators.Regex pattern

    member x.Text(?value, ?attributes: (string * string) list, ?required: bool, ?maxlength: int, ?pattern: string) =
        x.iText(value, attributes, required, maxlength, pattern)

    member private x.iFloat(value: float option, attributes: _ list option, required: bool option, maxlength: int option, min: float option, max: float option) =
        let value = Option.map string value
        let formlet = 
            x.iText(value, attributes, required, maxlength, None)
            |> validators.Float
        let formlet =
            match min,max with
            | Some min, Some max -> formlet |> validators.InRange min max
            | Some min, None -> formlet |> validators.GreaterOrEqual min
            | None, Some max -> formlet |> validators.LessOrEqual max
            | _ -> formlet
        formlet

    member x.Float(?value, ?attributes, ?required, ?maxlength, ?min, ?max) =
        x.iFloat(value, attributes, required, maxlength, min, max)

    member x.iInt(value: int option, attributes, required, maxlength, min, max) =
        let value = Option.map string value
        let formlet = 
            x.iText(value, attributes, required, maxlength, None)
            |> validators.Int
        let formlet =
            match min,max with
            | Some min, Some max -> formlet |> validators.InRange min max
            | Some min, None -> formlet |> validators.GreaterOrEqual min
            | None, Some max -> formlet |> validators.LessOrEqual max
            | _ -> formlet
        formlet

    member x.Int(?value: int, ?attributes, ?required, ?maxlength, ?min, ?max) =
        x.iInt(value, attributes, required, maxlength, min, max)

    member x.FloatRange(min, max, ?value, ?attributes, ?required) =
        let attributes = defaultArg attributes []
        let attributes = attributes |> mergeAttr ["type","range"]
        x.iFloat(value, Some attributes, required, None, Some min, Some max)

    member x.IntRange(min, max, ?value, ?attributes, ?required) =
        let attributes = defaultArg attributes []
        let attributes = attributes |> mergeAttr ["type","range"]
        x.iInt(value, Some attributes, required, None, Some min, Some max)

    member x.Url(?value, ?attributes, ?required) =
        x.iText(value, attributes, required, None, None)
        |> validators.Url

    member x.Email(?value, ?attributes, ?required) =
        x.iText(value, attributes, required, None, None)
        |> validators.Email

    member x.Radio(value, choices) =
        Formlet.radio value choices

    member x.Radio(value, choices) =
        Formlet.radioA value choices

    member x.Select(value, choices) =
        Formlet.select value choices

    member x.Select(value, choices) =
        Formlet.selectA value choices

    member x.SelectMulti(value, choices) =
        Formlet.selectMulti value choices

    member x.SelectMulti(value, choices) =
        Formlet.selectMultiA value choices

    member x.File(?attributes) =
        let attributes = defaultArg attributes []
        Formlet.file attributes

    member x.Hidden(?value) =
        let value = defaultArg value ""
        Formlet.hidden value

    member x.Password(?required) =
        Formlet.password |> Option.mapBoolOrId validators.Required required

    member x.Search(?value, ?attributes, ?required, ?maxlength, ?pattern) =
        x.iText(value, attributes, required, maxlength, pattern)

    member x.Tel(?value, ?attributes, ?required, ?maxlength, ?pattern) =
        x.iText(value, attributes, required, maxlength, pattern)

    member private x.iDateTime validator (value, attributes, required, min, max) =
        let value = Option.map dateTimeSerializer.Serialize value
        let attributes = defaultArg attributes []
        x.iText(value, Some attributes, required, None, None)
        |> validator min max

    member x.DateTime(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime validators.DateTime (value, attributes, required, min, max)

    member x.Date(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime validators.Date (value, attributes, required, min, max)

    member x.Month(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime validators.Month (value, attributes, required, min, max)

    member x.Week(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime validators.Week (value, attributes, required, min, max)

    member x.Time(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime validators.Time (value, attributes, required, min, max)

    member x.Submit(?value, ?attributes) =
        let attributes = defaultArg attributes []
        let attributes = attributes |> mergeAttr ["type", "submit"]
        let value = defaultArg value ""
        Formlet.input value attributes

    member private x.AddOrGetId f =
        match getId f with
        | Some id -> id,f
        | _ ->
            let id = "e" + Guid.NewGuid().ToString()
            let f = f |> mergeAttributes ["id",id]
            id, f

    member x.WithLabel text (f: _ Formlet) =
        let id,f = x.AddOrGetId f
        labelFor id text *> f

    member x.WithLabelRaw xml (f: _ Formlet) =
        let id,f = x.AddOrGetId f
        labelForRaw id xml *> f
