namespace Formlets

open System

/// <summary>
/// Validated form elements.
/// These generate the appropriate HTML5 element so you can also use HTML5 for client-side validation
/// </summary>
type FormElements(validators: IValidationFunctions) =
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

    member x.Select(value, choices, ?attr) =
        let attr = defaultArg attr []
        Formlet.select value choices attr

    member x.Select(value, choices, ?attr) =
        let attr = defaultArg attr []
        Formlet.selectA value choices attr

    member x.SelectMulti(value, choices, ?attr) =
        let attr = defaultArg attr []
        Formlet.selectMulti value choices attr

    member x.SelectMulti(value, choices, ?attr) =
        let attr = defaultArg attr []
        Formlet.selectMultiA value choices attr

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

    member private x.iDateTime (serializer: _ ISerializer) validator (value, attributes, required, min, max) =
        let value = Option.map serializer.Serialize value
        let attributes = defaultArg attributes []
        x.iText(value, Some attributes, required, None, None)
        |> validator min max

    member x.DateTime(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime dateTimeSerializer validators.DateTime (value, attributes, required, min, max)

    member x.DateTimeLocal(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime localDateTimeSerializer validators.DateTimeLocal (value, attributes, required, min, max)

    member x.Date(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime dateSerializer validators.Date (value, attributes, required, min, max)

    member x.Month(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime monthSerializer validators.Month (value, attributes, required, min, max)

    member x.Week(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime weekSerializer validators.Week (value, attributes, required, min, max)

    member x.Time(?value, ?attributes, ?required, ?min, ?max) =
        x.iDateTime timeSerializer validators.Time (value, attributes, required, min, max)

    member x.Submit(?value, ?attributes) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        Formlet.submit value attributes

    member x.Image(src, alt, ?attributes) =
        let attributes = defaultArg attributes []
        Formlet.image src alt attributes

    member x.Color(?value, ?attributes) =
        let attributes = defaultArg attributes []
        let attributes = ("type","color")::attributes
        let value = defaultArg value System.Drawing.Color.Black
        let value = colorSerializer.Serialize value
        Formlet.input value attributes
        |> validators.Color

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
