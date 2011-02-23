namespace Formlets

open System

type FormElements(validators: IValidate) =
    let toString o = o.ToString()
    member x.Validate = validators

    member x.Checkbox(value, ?required, ?attributes) =
        let attributes = defaultArg attributes []
        let formlet = Formlet.checkbox value attributes
        let formlet = 
            match required with
            | Some true -> formlet |> validators.Required
            | _ -> formlet
        formlet
   
    member x.Textarea(?value, ?attributes: (string * string) list) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        Formlet.textarea value attributes

    member private x.iText(value: string option, attributes: (string*string) list option, required: bool option, maxlength: int option, pattern: string option) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        let formlet = Formlet.input value attributes
        let formlet = 
            match maxlength with
            | Some v -> formlet |> validators.Maxlength v
            | _ -> formlet
        let formlet =
            match required with
            | Some true -> formlet |> validators.Required
            | _ -> formlet
        let formlet =
            match pattern with
            | Some v -> formlet |> validators.Regex v
            | _ -> formlet
        formlet

    member x.Text(?value, ?attributes: (string * string) list, ?required: bool, ?maxlength: int, ?pattern: string) =
        x.iText(value, attributes, required, maxlength, pattern)

    member private x.iFloat(value: float option, attributes: _ list option, required: bool option, maxlength: int option, min: float option, max: float option) =
        let value = Option.map toString value
        let formlet = 
            x.iText(value, attributes, required, maxlength, None)
            |> validators.Float
        let formlet =
            match min,max with
            | Some min, Some max -> formlet |> validators.InRangeFloat min max
            | Some min, None -> formlet |> validators.GreaterOrEqualFloat min
            | None, Some max -> formlet |> validators.LessOrEqualFloat max
            | _ -> formlet
        formlet

    member x.Float(?value, ?attributes, ?required, ?maxlength, ?min, ?max) =
        x.iFloat(value, attributes, required, maxlength, min, max)

    member x.Int(?value: int, ?attributes, ?required, ?maxlength, ?min, ?max) =
        let value = Option.map toString value
        let formlet = 
            x.iText(value, attributes, required, maxlength, None)
            |> validators.Int
        let formlet =
            match min,max with
            | Some min, Some max -> formlet |> validators.InRangeInt min max
            | Some min, None -> formlet |> validators.GreaterOrEqualInt min
            | None, Some max -> formlet |> validators.LessOrEqualInt max
            | _ -> formlet
        formlet

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
        match required with
        | Some true -> Formlet.password |> validators.Required
        | _ -> Formlet.password

    member x.WithLabel text (f: _ Formlet) =
        let id,f = 
            match getId f with
            | Some id -> id,f
            | _ -> 
                let id = "e" + Guid.NewGuid().ToString()
                let f = f |> mergeAttributes ["id",id]
                id, f
        labelFor id text *> f