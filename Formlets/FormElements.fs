namespace Formlets

open System

type FormElements(validators: Validate) =
    member x.Validate = validators

    member x.Checkbox(value, ?attributes) =
        let attributes = defaultArg attributes []
        Formlet.checkbox value attributes
   
    member x.Textarea(?value, ?attributes: (string * string) list) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        Formlet.textarea value attributes

    member private x.iText(value: string option, attributes: (string*string) list option, required: bool option, size: int option, maxlength: int option, pattern: string option) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        let required = defaultArg required false
        let requiredAttr = if required then ["required",""] else []
        let size = match size with Some v -> ["size",v.ToString()] | _ -> []
        let maxlength = match maxlength with Some v -> ["maxlength",v.ToString()] | _ -> []
        let patternAttr = match pattern with Some v -> ["pattern",v] | _ -> []
        let attributes = attributes |> mergeAttr (requiredAttr @ size @ maxlength @ patternAttr)
        let formlet = Formlet.input value attributes
        let formlet =
            if required
                then formlet |> validators.Required
                else formlet
        let formlet =
            match pattern with
            | Some v -> formlet |> validators.Regex v
            | _ -> formlet
        formlet

    member x.Text(?value, ?attributes: (string * string) list, ?required: bool, ?size: int, ?maxlength: int, ?pattern: string) =
        x.iText(value, attributes, required, size, maxlength, pattern)

    member private x.iNumber(value: float option, attributes: _ list option, required: bool option, size: int option, maxlength: int option, min: float option, max: float option, errorMsg: (string -> string) option, rangeErrorMsg: ((float option * float option) -> float -> string) option) =
        let value = match value with Some v -> Some <| v.ToString() | _ -> None
        let errorMsg = defaultArg errorMsg (fun _ -> "Invalid number")
        let minAttr = match min with Some v -> ["min",v.ToString()] | _ -> []
        let maxAttr = match max with Some v -> ["max",v.ToString()] | _ -> []
        let defaultRangeErrorMsg (min,max) v =
            match min,max with
            | Some min, Some max -> sprintf "Value must be between %f and %f" min max
            | Some min, None -> sprintf "Value must be higher than %f" min
            | None, Some max -> sprintf "Value must be lower than %f" max
            | _, _ -> ""            
        let rangeErrorMsg = defaultArg rangeErrorMsg defaultRangeErrorMsg
        let rangeErrorMsg = rangeErrorMsg (min,max)
        let rangeValidator v =
            match min,max with
            | Some min, Some max -> v >= min && v <= max
            | Some min, None -> v >= min
            | None, Some max -> v <= max
            | _,_ -> true
        x.iText(value, attributes, required, size, maxlength, None)
        |> mergeAttributes (["type","number"] @ minAttr @ maxAttr)
        |> validators.Float
        |> map float
        |> satisfies (validators.BuildValidator rangeValidator rangeErrorMsg)

    member x.Number(?value, ?attributes, ?required, ?size, ?maxlength, ?min, ?max, ?errorMsg) =
        x.iNumber(value, attributes, required, size, maxlength, min, max, errorMsg, None)

    member x.Int(?value, ?attributes, ?required, ?size, ?maxlength, ?min, ?max, ?errorMsg) =
        let value = Option.map float value
        let min = Option.map float min
        let max = Option.map float max
        let errorMsg2 (i: float) =
            match errorMsg with
            | Some f -> i.ToString() |> f
            | _ -> "Invalid number"
        let defaultRangeErrorMsg (min,max) v =
            let min = Option.map int min
            let max = Option.map int max
            match min,max with
            | Some min, Some max -> sprintf "Value must be between %d and %d" min max
            | Some min, None -> sprintf "Value must be higher than %d" min
            | None, Some max -> sprintf "Value must be lower than %d" max
            | _, _ -> ""
        x.iNumber(value, attributes, required, size, maxlength, min, max, errorMsg, Some defaultRangeErrorMsg)
        |> satisfies (validators.BuildValidator (fun n -> Math.Truncate n = n) errorMsg2)
        |> map int

    member x.Url(?value, ?attributes, ?required) =
        x.iText(value, attributes, required, None, None, None)
        |> mergeAttributes ["type","url"]
        |> validators.Url

    member x.Email(?value, ?attributes, ?required) =
        x.iText(value, attributes, required, None, None, None)
        |> mergeAttributes ["type","email"]
        |> validators.Email

    member x.WithLabel text (f: _ Formlet) =
        let id,f = 
            match getId f with
            | Some id -> id,f
            | _ -> 
                let id = "e" + Guid.NewGuid().ToString()
                let f = f |> mergeAttributes ["id",id]
                id, f
        labelFor id text *> f