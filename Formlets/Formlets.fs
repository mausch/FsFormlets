namespace Formlets

(*
Formlets implementation based on http://groups.inf.ed.ac.uk/links/formlets/
TODO:
* extend to use with querystring
* default values for form elements
* inline error messages
* change return type to Success | Fail | Invalid instead of Some | None (Error applicative)
* radio and select: accept any type as value, not just strings, map them dynamically to string and back
*)

open System
open System.Collections.Specialized
open System.Web

type 'a Validator = ('a -> bool) * ('a -> xml_item list -> xml_item list)
type 'a ValidationResult =
    | Pass of 'a
    | Fail of 'a
    | Dead

type private 'a EO = 'a Error Environ
type private 'a AEO = 'a EO XmlWriter
type private 'a AE = 'a Environ XmlWriter
type private 'a AO = 'a Error XmlWriter
type private 'a NAE = 'a AE NameGen
type private 'a EAO = 'a AO Environ
type private 'a AEAO = 'a EAO XmlWriter
type 'a Formlet = 'a Error XmlWriter Environ XmlWriter NameGen

module Formlet =

    // AE = Compose (XmlWriter) (Environment)
    let private ae_pure x : 'a AE = XmlWriter.puree (Environ.puree x)
    let private ae_ap (f: ('a -> 'b) AE) (x: 'a AE) : 'b AE =
        (XmlWriter.lift2 Environ.ap) f x

    // NAE = Compose (NameGen) (AE)
    let private nae_pure x : 'a NAE = NameGen.puree (ae_pure x)
    let private nae_ap (f: ('a -> 'b) NAE) (x: 'a NAE) : 'b NAE =
        (NameGen.lift2 ae_ap) f x
    let private nae_lift (f: 'a -> 'b) (x: 'a NAE) : 'b NAE = 
        nae_ap (nae_pure f) x

    // AO = Compose (XmlWriter) (Error)
    let private ao_pure x : 'a AO = XmlWriter.puree (Error.puree x)
    let private ao_ap (f: ('a -> 'b) AO) (x: 'a AO) : 'b AO = 
        (XmlWriter.lift2 Error.ap) f x

    // EAO = Compose (Environ) (AO)
    let private eao_pure x : 'a EAO = Environ.puree (ao_pure x)
    let private eao_ap (f: ('a -> 'b) EAO) (x: 'a EAO) : 'b EAO =
        (Environ.lift2 ao_ap) f x

    // AEAO = Compose (XmlWriter) (EAO)
    let private aeao_pure x: 'a AEAO = XmlWriter.puree (eao_pure x)
    let private aeao_ap (f: ('a -> 'b) AEAO) (x: 'a AEAO) : 'b AEAO =
        (XmlWriter.lift2 eao_ap) f x

    // Compose (NameGen) (AEAO)
    let puree x : 'a Formlet = NameGen.puree (aeao_pure x)
    let ap (f: ('a -> 'b) Formlet) (x: 'a Formlet) : 'b Formlet = 
        (NameGen.lift2 aeao_ap) f x

    let inline (<*>) f x = ap f x
    let inline lift f a = puree f <*> a

    /// Convenience 'lift' with flipped parameters
    let inline (|>>) x f = lift f x
    let inline lift2 f a b = puree f <*> a <*> b
    let inline lift3 f a b c = puree f <*> a <*> b <*> c
    let inline lift4 f a b c d = puree f <*> a <*> b <*> c <*> d

    /// Sequence actions, discarding the value of the first argument.
    let inline apr x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = apr x y
    /// Sequence actions, discarding the value of the second argument.
    let inline apl x y = lift2 (fun z _ -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline (<*) x y = apl x y
    let inline pair a b = lift2 (fun x y -> x,y) a b
    let inline ( **) a b = pair a b

    let inline yields x = puree x // friendly alias

    let xml x : unit Formlet = 
        let v = XmlWriter.xml x
        let xml1 = XmlWriter.lift Error.puree v |> Environ.puree
        let xml2 = XmlWriter.lift (fun _ -> xml1) v
        NameGen.puree xml2

    let nop = puree ()
    let text s : unit Formlet = xml [Text s]
    let tag name attributes (f: 'a Formlet) : 'a Formlet = 
        let xtag x = XmlWriter.tag name attributes x
        let xml1 x = XmlWriter.lift id (xtag x)
        let xml2 x = Environ.lift xml1 x
        let xml3 x = XmlWriter.lift xml2 (xtag x)
        NameGen.lift xml3 f

    let run (v: 'a Formlet) : EnvDict -> (xml_item list * 'a option)  = 
        NameGen.run v |> snd

    let renderToNodes (v: _ Formlet): xml_item list = 
        NameGen.run v |> fst
    
    /// Renders a formlet to XDocument
    let renderToXml (v: _ Formlet) = 
        v |> renderToNodes |> XmlWriter.render

    /// Renders a formlet to string
    let render (v: _ Formlet) = 
        let x = renderToXml v
        x.ToString()

    // Validation functions

    let private check (validator: 'a Validator) (a: 'a AO) : 'a AO =
        let result: 'a ValidationResult XmlWriter =
            let errorToValidationResult o =
                let pred = fst validator
                let check' p v =
                    if p v
                        then Pass v
                        else Fail v
                let liftedCheck = Error.lift (check' pred)
                match liftedCheck o with
                | Some v -> v
                | _ -> Dead
            XmlWriter.lift errorToValidationResult a
        let validationResultToError: 'b ValidationResult -> 'b Error = 
            function 
            | Pass v -> Error.puree v 
            | _ -> Error.failure
        let w = XmlWriter.lift validationResultToError result
        let errorMsg = snd validator
        match result with
        | x, Fail v -> XmlWriter.plug (errorMsg v) w
        | x -> w

    /// Applies a validator to a formlet
    let satisfies (validator: 'a Validator) (f: 'a Formlet) : 'a Formlet =
        nae_lift (check validator) f

    /// Constructs a validator
    let err (isValid: 'a -> bool) (errorMsg: 'a -> string) : 'a Validator = 
        let addError value xml = 
            [
                Tag("span", ["class","errorinput"], xml)
                Tag("span", ["class","error"], [Text(errorMsg value)])
            ]
        isValid, addError

    /// Constructs a validator from a regular expression
    let errx (rx: string) (errorMsg: string -> string) : string Validator =
        let v value = System.Text.RegularExpressions.Regex(rx).IsMatch(value)
        err v errorMsg

    // helper functions

    /// Builds a pair (2-tuple)
    let t2 a b = a,b

    /// Builds a triple (3-tuple)
    let t3 a b c = a,b,c

    /// Builds a 4-tuple
    let t4 a b c d = a,b,c,d

    /// Builds a 5-tuple
    let t5 a b c d e = a,b,c,d,e

    /// Builds a 6-tuple
    let t6 a b c d e f = a,b,c,d,e,f

    /// Builds a 7-tuple
    let t7 a b c d e f g = a,b,c,d,e,f,g

    /// Builds a 8-tuple
    let t8 a b c d e f g h = a,b,c,d,e,f,g,h

    // Generic HTML functions

    let generalElement nameGen defaultValue (tag: string -> InputValue list -> xml_item list): InputValue list Formlet =
        let t name = 
            let xml = tag name
            let eao = 
                fun env -> 
                    let value = Environ.lookup name env
                    let xml = xml value
                    xml,Some value
            XmlWriter.plug (fun _ -> xml defaultValue) (XmlWriter.puree eao)
        NameGen.lift t nameGen
            
    let generalGeneratedElement x = generalElement NameGen.nextName x
    let generalAssignedElement name = generalElement (NameGen.puree name)

    let extractOptionString =
        function
        | [] -> None
        | [x] -> 
            match x with
            | Value v -> Some v
            | _ -> failwith "Unexpected file"
        | _ -> failwith "Unexpected multiple values"

    let extractOptional (f: InputValue list Formlet) : string option Formlet =
        lift extractOptionString f

    let extractString (f: InputValue list Formlet) : string Formlet =
        extractOptional f |> lift Option.get

    let extractStrings (f: InputValue list Formlet) : string list Formlet =
        let extractOne =
            function
            | Value v -> v
            | _ -> failwith "Unexpected file"
        lift (List.map extractOne) f

    let getValueAttr =
        function
        | [v] -> 
            match v with
            | Value v -> ["value", v]
            | _ -> failwith "file not expected"
        | _ -> []

    let optionalInput defaultValue attributes: string option Formlet =
        let tag name (boundValue: InputValue list) = 
            let valueAttr = getValueAttr boundValue
            [Tag("input", ["name", name] @ valueAttr @ attributes, [])]
        generalGeneratedElement [Value defaultValue] tag |> extractOptional

    // Concrete HTML functions

    let input value attributes : string Formlet = 
        let tag name boundValue = 
            let valueAttr = getValueAttr boundValue
            [Tag("input", ["name", name] @ valueAttr @ attributes, [])]
        generalGeneratedElement [Value value] tag |> extractString

    let assignedInput name value attributes : string Formlet =
        let tag name boundValue = 
            let valueAttr = getValueAttr boundValue
            [Tag("input", ["name", name] @ valueAttr @ attributes, [])]
        generalAssignedElement name [Value value] tag |> extractString

    let password : string Formlet = 
        input "" ["type","password"]

    let hidden value: string Formlet = 
        input value ["type","hidden"]

    let assignedHidden name value : string Formlet =
        assignedInput name value ["type","hidden"]

    let checkbox on : bool Formlet =
        let transform = 
            function
            | None -> false
            | Some _ -> true
        let tag name (boundValue: InputValue list) = 
            let value = extractOptionString boundValue
            let valueAttr = 
                match value with
                | Some x -> ["checked","checked"]
                | _ -> []
            [Tag("input", ["name",name; "type","checkbox"] @ valueAttr, [])]
        let on = if on then [Value ""] else []
        generalGeneratedElement on tag 
        |> extractOptional
        |> lift transform

    let radio selected (choices: (string*string) seq): string Formlet =
        let makeLabel id text = 
            Tag("label", ["for", id], [Text text])
        let makeRadio name value id selected = 
            let on = if selected then ["checked","checked"] else []
            Tag("input", ["type","radio"; "name",name; "id",id; "value",value] @ on, [])
        let tag name boundValue = 
            let selectedValue = extractOptionString boundValue |> Option.get
            choices 
            |> Seq.zip {1..Int32.MaxValue} 
            |> Seq.map (fun (i,(value,label)) -> name, value, label, name + "_" + i.ToString(), selectedValue = value)
            |> Seq.collect (fun (name,value,label,id,selected) -> [makeRadio name value id selected; makeLabel id label])
            |> Seq.toList
        generalGeneratedElement [Value selected] tag
        |> extractString

    let internal makeOption selected (value,text) = 
        let on = 
            if Seq.exists ((=) value) selected
                then ["selected","selected"] 
                else []
        Tag("option", ["value",value] @ on, [Text text])
    let internal makeSelect name attr options = 
        Tag("select", ["name",name] @ attr, options)
    let internal selectTag selected choices attr name boundValue =
        [choices |> Seq.map (makeOption selected) |> Seq.toList |> makeSelect name attr]

    let select selected (choices: (string*string) seq): string Formlet = 
        generalGeneratedElement [] (selectTag [selected] choices [])
        |> extractString

    let selectMulti selected (choices: (string*string) seq): string list Formlet = 
        generalGeneratedElement [] (selectTag selected choices ["multiple","multiple"]) 
        |> extractStrings

    let generalTextarea elemBuilder value (rows: int option) (cols: int option) : string Formlet = 
        let attributes = 
            let rows = match rows with Some r -> ["rows",r.ToString()] | _ -> []
            let cols = match cols with Some r -> ["cols",r.ToString()] | _ -> []
            rows @ cols
        let tag name boundValue = 
            let content =
                match boundValue with
                | [v] -> 
                    match v with
                    | Value v -> v
                    | _ -> failwith "file not expected"
                | _ -> ""
            [Tag("textarea", ["name",name] @ attributes, [Text content])]
        elemBuilder value tag
        |> extractString

    let textarea value = generalTextarea generalGeneratedElement [Value value]

    let assignedTextarea name value = generalTextarea (generalAssignedElement name) [Value value]

    let file : HttpPostedFileBase option Formlet = 
        let tag name boundValue = 
            [Tag("input", ["type", "file"; "name", name], [])]
        let fileOnly =
            function
            | Some (File f) -> Some f
            | None -> None
            | _ -> failwith "File expected, got value instead"
        let r = generalGeneratedElement [] tag
        lift (Seq.nth 0 >> Some >> fileOnly) r
            
    let form hmethod haction attributes (v: 'a Formlet) : 'a Formlet = 
        tag "form" (["method",hmethod; "action",haction] @ attributes) v

    let submit n = tag "input" ["type","submit"; "value",n] nop

    let br = tag "br" [] nop

    let iframe src attr = xml [Tag("iframe", (attr @ ["src",src]), [])]
    let noscript x = tag "noscript" [] x
    let script src = tag "script" ["type","text/javascript"; "src",src] nop

    type ReCaptchaSettings = {
        PublicKey: string
        PrivateKey: string
        MockedResult: bool option
    }

    let reCaptcha (settings: ReCaptchaSettings) requestIP = 
        let validate (challenge, response) =
            match settings.MockedResult with
            | Some v -> v
            | _ ->
                let nv = NameValueCollection.fromSeq ["privatekey",settings.PrivateKey; "remoteip",requestIP; "challenge",challenge; "response",response]
                use http = new System.Net.WebClient()
                let bytes = http.UploadValues("http://www.google.com/recaptcha/api/verify", nv)
                (System.Text.Encoding.UTF8.GetString bytes).Split('\n').[0] = "true"
        
        script (sprintf "http://www.google.com/recaptcha/api/challenge?k=%s" settings.PublicKey)
        *> noscript (
            yields t2
            <*> iframe (sprintf "http://www.google.com/recaptcha/api/noscript?k=%s" settings.PublicKey) ["height","300"; "width","500"; "frameborder","0"] 
            *> assignedTextarea "recaptcha_challenge_field" "" (Some 3) (Some 40)
            <*> assignedHidden "recaptcha_response_field" "manual_challenge"
        )
        |> satisfies (err validate (fun (_,_) -> "Invalid captcha"))
        |> lift ignore

