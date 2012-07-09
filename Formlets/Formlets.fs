namespace Formlets

(*
Formlets implementation based on http://groups.inf.ed.ac.uk/links/formlets/
TODO:
* change return type to Success | Fail | Invalid instead of Some | None (Error applicative)
*)

open System
open System.Collections.Generic
open System.Collections.Specialized
open System.Xml.Linq
open System.Web
open FSharpx

type 'a Validator = {
    IsValid: 'a -> bool
    ErrorForm: 'a -> XNode list -> XNode list
    ErrorList: 'a -> string list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Validator =
    let isValid (v: _ Validator) = v.IsValid
    let errorForm (v: _ Validator) = v.ErrorForm
    let errorList (v: _ Validator) = v.ErrorList
    let toTuple (v: _ Validator) = v.IsValid, v.ErrorForm, v.ErrorList

type 'a ValidationResult =
    | Pass of 'a
    | Fail of 'a
    | Dead

type 'a FormletResult = {
    /// Form with collected values and rendered errors
    Form: XNode list
    /// Error list
    Errors: string list 
    /// Collected value
    Value: 'a option
}
    

type private 'a LO = 'a option ErrorList
type private 'a ELO = 'a LO Environ
type private 'a AELO = 'a ELO XmlWriter
type private 'a AE = 'a Environ XmlWriter
type private 'a AO = 'a option XmlWriter
type private 'a NAE = 'a AE NameGen
type private 'a EAO = 'a AO Environ
type private 'a ALO = 'a LO XmlWriter
type private 'a EALO = 'a ALO Environ
type private 'a AEALO = 'a EALO XmlWriter
type 'a Formlet = 'a option ErrorList XmlWriter Environ XmlWriter NameGen

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Formlet =
    open FSharpx
    open Formlets.Helpers

    let inline private ae_pure x : 'a AE = XmlWriter.puree (Environ.puree x)
    let private ae_ap (x: 'a AE) (f: ('a -> 'b) AE) : 'b AE =
        XmlWriter.lift2 Environ.ap x f

    let inline private nae_pure x : 'a NAE = NameGen.puree (ae_pure x)
    let inline private nae_ap (x: 'a NAE) (f: ('a -> 'b) NAE) : 'b NAE =
        NameGen.lift2 ae_ap x f
    let private nae_map (f: 'a -> 'b) (x: 'a NAE) : 'b NAE = 
        nae_pure f |> nae_ap x

    let inline private ao_pure x : 'a AO = XmlWriter.puree (Some x)
    let private ao_ap (x: 'a AO) (f: ('a -> 'b) AO) : 'b AO = 
        XmlWriter.lift2 Option.ap x f

    let inline private eao_pure x : 'a EAO = Environ.puree (ao_pure x)
    let private eao_ap (x: 'a EAO) (f: ('a -> 'b) EAO) : 'b EAO =
        Environ.lift2 ao_ap x f

    let inline private lo_pure x : 'a LO = ErrorList.puree (Some x)
    let inline private lo_ap (x: 'a LO) (f: ('a -> 'b) LO) : 'b LO =
        ErrorList.lift2 Option.ap x f
    let private lo_map (f: 'a -> 'b) (x: 'a LO) : 'b LO =
        lo_pure f |> lo_ap x

    let inline private alo_pure x : 'a ALO = XmlWriter.puree (lo_pure x)
    let private alo_ap (x: 'a ALO) (f: ('a -> 'b) ALO) : 'b ALO =
        XmlWriter.lift2 lo_ap x f

    let inline private ealo_pure x : 'a EALO = Environ.puree (alo_pure x)
    let private ealo_ap (x: 'a EALO) (f: ('a -> 'b) EALO) : 'b EALO =
        Environ.lift2 alo_ap x f

    let inline private aealo_pure x: 'a AEALO = XmlWriter.puree (ealo_pure x)
    let private aealo_ap (x: 'a AEALO) (f: ('a -> 'b) AEALO) : 'b AEALO =
        XmlWriter.lift2 ealo_ap x f

    let puree x : 'a Formlet = NameGen.puree (aealo_pure x)
    let ap (x: 'a Formlet) (f: ('a -> 'b) Formlet) : 'b Formlet = 
        NameGen.lift2 aealo_ap x f

    let inline (<*>) f x = ap x f
    let inline map f a = puree f <*> a

    /// Convenience 'map' with flipped parameters
    let inline (|>>) x f = map f x
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

    let private mapXml (v: 'a XmlWriter) : 'a Formlet =
        let xml1 = XmlWriter.map (Some >> ErrorList.puree) v |> Environ.puree
        let xml2 = XmlWriter.map (fun _ -> xml1) v
        NameGen.puree xml2

    /// Maps a xml forest to formlet
    let xml x : unit Formlet = 
        XmlWriter.xml x |> mapXml

    /// Parses a raw xml forest to formlet
    let rawXml x : unit Formlet =
        XmlWriter.rawXml x |> mapXml

    /// maps a xml tree to formlet
    let inline xnode (e: XNode) : unit Formlet = xml [e]

    /// No-operation formlet
    let nop = puree ()

    /// Maps text to formlet
    let inline text (s: string) : unit Formlet = xml [XText s]

    /// Maps formatted text to formlet
    let inline textf fmt = Printf.ksprintf text fmt

    /// <summary>
    /// maps a HTML tag to wrap a formlet
    /// </summary>
    /// <param name="name">Tag name</param>
    /// <param name="attributes">Element attributes</param>
    /// <param name="f">Inner formlet</param>
    let tag name attributes (f: 'a Formlet) : 'a Formlet = 
        let xtag x = XmlWriter.tag name attributes x
        let xml1 x = XmlWriter.map id (xtag x)
        let xml2 x = Environ.map xml1 x
        let xml3 x = XmlWriter.map xml2 (xtag x)
        NameGen.map xml3 f

    /// Runs a formlet.
    /// Returns a populated form with error messages and the result value
    let inline run (formlet: _ Formlet) env = 
        let form, (errors, value) = (NameGen.run formlet |> snd) env
        { FormletResult.Form = form
          Errors = errors
          Value = value }

    /// Simplified view of formlet result
    let (|Success|Failure|) (a: _ FormletResult) =
        match a with
        | { FormletResult.Value = Some v } -> Success v
        | _ -> Failure(a.Form, a.Errors)

    let inline runToChoice f env =
        match run f env with
        | Success v -> Choice1Of2 v
        | Failure e -> Choice2Of2 e
    
    /// Renders a formlet to XNode
    let inline renderToXml (v: _ Formlet) = 
        NameGen.run v |> fst

    /// Renders a formlet to string
    let inline render f = f |> renderToXml |> XmlWriter.render

    let inline mergeAttributes (a: (string * string) seq) (f: 'a Formlet) : 'a Formlet =
        let merge x = XmlWriter.mergeAttr a x
        let xml1 x = XmlWriter.map id (merge x)
        let xml2 x = Environ.map xml1 x
        let xml3 x = XmlWriter.map xml2 (merge x)
        NameGen.map xml3 f

    let inline getId f = f |> NameGen.run |> XmlWriter.getId

    // Validation functions

    let private check (validator: 'a Validator) (a: 'a ALO) : 'a ALO =
        let result: 'a ValidationResult XmlWriter =
            let errorToValidationResult (o: 'a LO) =
                let check' p v =
                    if p v
                        then Pass v
                        else Fail v
                let mappedCheck = lo_map (check' validator.IsValid)
                match mappedCheck o with
                | _,Some v -> v
                | _ -> Dead
            XmlWriter.map errorToValidationResult a
        let validationResultToError: 'b ValidationResult -> 'b LO = 
            function 
            | Pass v -> lo_pure v
            | _ -> ErrorList.puree None
        let w = XmlWriter.map validationResultToError result
        match result with
        | x, Fail v -> 
            let w =
                let append a = ErrorList.append (validator.ErrorList v) a
                XmlWriter.map append w
            XmlWriter.plug (validator.ErrorForm v) w
        | x -> w

    /// Applies a validator to a formlet
    let satisfies (validator: 'a Validator) (f: 'a Formlet) : 'a Formlet =
        nae_map (check validator) f

    /// <summary>
    /// Constructs a validator
    /// </summary>
    /// <param name="isValid">Determines if value is valid</param>
    /// <param name="errorMsg">Builds the error message</param>
    let err (isValid: 'a -> bool) (errorMsg: 'a -> string) : 'a Validator = 
        let addError value xml = 
            let elems = 
                [
                    XmlHelpers.xelem "span" ["class","errorinput"] xml
                    XmlHelpers.xelem "span" ["class","error"] [XText(errorMsg value)]
                ]
            List.map (fun e -> e :> XNode) elems
        { IsValid = isValid
          ErrorForm = addError
          ErrorList = errorMsg >> List.singleton }

    // Generic HTML functions

    let generalElement nameGen defaultValue (tag: string -> InputValue list -> XNode list): InputValue list Formlet =
        let t name = 
            let tag = tag name
            let ealo = 
                fun env -> 
                    let value = Environ.lookup name env
                    let dvalue = if value.Length = 0 then defaultValue else value
                    let xml = tag dvalue
                    xml,([],Some value)
            XmlWriter.plug (fun _ -> tag defaultValue) (XmlWriter.puree ealo)
        NameGen.map t nameGen
            
    let generalGeneratedElement x = generalElement NameGen.nextName x
    let generalAssignedElement name = generalElement (NameGen.puree name)

    let extractOptionString =
        function
        | [] -> None
        | [Value v] -> Some v
        | [File _] -> failwith "Unexpected file"
        | _ -> failwith "Unexpected multiple values"

    let extractOptional (f: InputValue list Formlet) : string option Formlet =
        map extractOptionString f

    let extractString (f: InputValue list Formlet) : string Formlet =
        extractOptional f |> map Option.get

    let extractStrings (f: InputValue list Formlet) : string list Formlet =
        let extractOne =
            function
            | Value v -> v
            | _ -> failwith "Value expected, got file instead"
        map (List.map extractOne) f

    let getValueAttr =
        function
        | [Value v] -> ["value", v]
        | [File _] -> failwith "Value expected, got file instead"
        | _ -> []

    let optionalInput defaultValue attributes: string option Formlet =
        let tag name (boundValue: InputValue list) = 
            let valueAttr = getValueAttr boundValue
            [XmlHelpers.xelem "input" (["name", name] @ valueAttr @ attributes) []]
        generalGeneratedElement [Value defaultValue] tag |> extractOptional

    let internal generatedField =
        let tag _ _ = []
        generalGeneratedElement [] tag

    let internal iassignedField name =
        let tag _ _ = []
        generalAssignedElement name [] tag

    // Concrete HTML functions

    let field = generatedField |> extractString
    let assignedField = iassignedField >> extractString
    let optionalField = generatedField |> extractOptional
    let optionalAssignedField = iassignedField >> extractOptional

    /// <summary>
    /// Creates a &lt;input type=&quot;text&quot;&gt; formlet
    /// </summary>
    /// <param name="value">Default value</param>
    /// <param name="attributes">Additional attributes</param>
    let input value attributes : string Formlet = 
        let tag name boundValue = 
            let valueAttr = getValueAttr boundValue
            [XmlHelpers.xelem "input" (["name", name] @ valueAttr @ attributes) []]
        generalGeneratedElement [Value value] tag |> extractString

    /// <summary>
    /// Creates a &lt;input type=&quot;text&quot;&gt; formlet with a manually assigned element name
    /// </summary>
    /// <param name="name">Form element name</param>
    /// <param name="value">Default value</param>
    /// <param name="attributes">Additional attributes</param>
    let assignedInput name value attributes : string Formlet =
        let tag name boundValue = 
            let valueAttr = getValueAttr boundValue
            [XmlHelpers.xelem "input" (["name", name] @ valueAttr @ attributes) []]
        generalAssignedElement name [Value value] tag |> extractString

    /// <summary>
    /// Creates a &lt;input type=&quot;password&quot;&gt; formlet 
    /// </summary>
    let password : string Formlet = 
        input "" ["type","password"]

    /// <summary>
    /// Creates a &lt;input type=&quot;hidden&quot;&gt; formlet 
    /// </summary>
    /// <param name="value">Default value</param>
    let hidden value: string Formlet = 
        input value ["type","hidden"]

    /// <summary>
    /// Creates a &lt;input type=&quot;hidden&quot;&gt; formlet with a manually assigned element name
    /// </summary>
    /// <param name="name">Form element name</param>
    /// <param name="value">Default value</param>
    let assignedHidden name value : string Formlet =
        assignedInput name value ["type","hidden"]

    /// <summary>
    /// Creates a &lt;input type=&quot;checkbox&quot;&gt; formlet
    /// </summary>
    /// <param name="on">Initial check value</param>
    let checkbox on attributes : bool Formlet =
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
            let attr = ["name",name; "type","checkbox"] @ valueAttr
            let attr = attributes |> mergeAttr attr
            [XmlHelpers.xelem "input" attr []]
        let on = if on then [Value ""] else []
        generalGeneratedElement on tag 
        |> extractOptional
        |> map transform

    let labelFor id text = 
        [XmlWriter.labelFor id text] |> xml

    let labelForRaw id x =
        [XmlWriter.labelRawFor id x] |> xml

    /// <summary>
    /// Creates a &lt;input type=&quot;radio&quot;&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected value</param>
    /// <param name="choices">Select options</param>
    let radio selected (choices: (string*string) seq): string Formlet =
        let makeRadio name value id selected = 
            let on = if selected then ["checked","checked"] else []
            XmlHelpers.xelem "input" (["type","radio"; "name",name; "id",id; "value",value] @ on) []
        let tag name boundValue = 
            let selectedValue = extractOptionString boundValue |> Option.get
            choices
            |> Seq.mapi (fun i (value,label) -> name, value, label, name + "_" + i.ToString(), selectedValue = value)
            |> Seq.collect (fun (name,value,label,id,selected) -> [makeRadio name value id selected; XmlWriter.labelFor id label])
            |> Seq.toList
        generalGeneratedElement [Value selected] tag
        |> extractString

    let buildHashMap (choices: ('a * string) seq) =
        let itemMap = choices |> Seq.map (fun (k,v) -> (hash k).ToString(),k,v) |> Seq.toList
        let mappedChoices = itemMap |> Seq.map (fun (h,_,t) -> h,t)
        let mapHashToValue v = 
            let _,value,_ = List.find (fun (h,_,_) -> h = v) itemMap
            value
        mappedChoices, mapHashToValue

    let radioA (selected: 'a) (choices: ('a * string) seq) : 'a Formlet =
        let mappedChoices, mapHashToValue = buildHashMap choices
        radio (hashs selected) mappedChoices
        |> map mapHashToValue

    let internal makeOption selected (value,text:string) = 
        let on = 
            if Seq.exists ((=) value) selected
                then ["selected","selected"] 
                else []
        XmlHelpers.xelem "option" (["value",value] @ on) [XText text]
    let internal makeSelect name attr options = 
        XmlHelpers.xelem "select" (["name",name] @ attr) options
    let internal selectTag selected choices attr name (boundValue: InputValue list) =
        let selected =
            match boundValue with
            | [] -> selected
            | _ -> boundValue |> Seq.map (function Value v -> v | _ -> failwithf "Value expected, file found at name %s" name)
        [choices |> Seq.map (makeOption selected) |> Seq.toList |> makeSelect name attr]

    /// <summary>
    /// Creates a &lt;select&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected value</param>
    /// <param name="choices">Select options</param>
    let select selected (choices: (string*string) seq) attr: string Formlet = 
        let tag = selectTag [selected] choices attr
        generalGeneratedElement [] tag
        |> extractString

    let selectA (selected: 'a) (choices: ('a * string) seq) attr =
        let mappedChoices, mapHashToValue = buildHashMap choices
        select (hashs selected) mappedChoices attr
        |> map mapHashToValue

    /// <summary>
    /// Creates a &lt;select multiple&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected values</param>
    /// <param name="choices">Select options</param>
    let selectMulti selected (choices: (string*string) seq) attr: string list Formlet = 
        let attr = ("multiple","multiple")::attr
        let tag = selectTag selected choices attr
        generalGeneratedElement [] tag
        |> extractStrings

    let selectMultiA (selected: 'a seq) (choices: ('a * string) seq) attr: 'a list Formlet =
        let mappedChoices, mapHashToValue = buildHashMap choices
        selectMulti (Seq.map hashs selected) mappedChoices attr
        |> map (List.map mapHashToValue)
    
    /// <summary>
    /// Creates a &lt;textarea&gt; formlet
    /// </summary>
    /// <param name="elemBuilder">Form element builder</param>
    /// <param name="value">Default value</param>
    /// <param name="attr">Element attributes</param>
    let internal generalTextarea elemBuilder value attr : string Formlet = 
        let tag name boundValue = 
            let content =
                match boundValue with
                | [v] -> 
                    match v with
                    | Value v -> v
                    | _ -> failwith "file not expected"
                | _ -> ""
            [XmlHelpers.xelem "textarea" (["name",name] @ attr) [XText content]]
        elemBuilder value tag
        |> extractString

    /// <summary>
    /// Creates a &lt;textarea&gt; formlet
    /// </summary>
    /// <param name="value">Default value</param>
    let textarea value = generalTextarea generalGeneratedElement [Value value]

    /// <summary>
    /// Creates a &lt;textarea&gt; formlet with a manually assigned name
    /// </summary>
    /// <param name="name">Form element name</param>
    /// <param name="value">Default value</param>
    let assignedTextarea name value = generalTextarea (generalAssignedElement name) [Value value]

    /// <summary>
    /// Creates an &lt;input type=&quot;file&quot;&gt; formlet
    /// </summary>
    let file attr : HttpPostedFileBase option Formlet = 
        let tag name boundValue = 
            [XmlHelpers.xelem "input" (["type", "file"; "name", name] @ attr) []]
        let r = generalGeneratedElement [] tag
        let oneFileOrNone =
            function
            | [] -> None
            | [File f] -> Some f
            | _ -> failwith "File expected, got value instead"
        map oneFileOrNone r

    /// <summary>
    /// Creates a &lt;form&gt; tag
    /// </summary>
    /// <param name="hmethod">Form method attribute</param>
    /// <param name="haction">Form action attribute</param>
    /// <param name="attributes">Additional attributes</param>
    /// <param name="v">Formlet</param>
    let form hmethod haction attributes (v: 'a Formlet) : 'a Formlet = 
        tag "form" (["method",hmethod; "action",haction] @ attributes) v

    /// <summary>
    /// Creates an &lt;input type=&quot;submit&quot;&gt; tag
    /// </summary>
    /// <param name="n">Submit value</param>
    /// <param name="attr">Additional attributes</param>
    let submit value attr : string option Formlet = 
        optionalInput value (["type","submit"] @ attr)

    /// <summary>
    /// Creates an &lt;input type=&quot;image&quot;&gt; formlet
    /// </summary>
    /// <param name="src">Image src</param>
    /// <param name="src">Image alt</param>
    /// <param name="attr">Additional attributes</param>
    let image src alt attr : System.Drawing.Point option Formlet = 
        let tag name = 
            [XmlHelpers.xelem "input" (["name",name; "type","image"; "src",src; "alt",alt] @ attr) []]
        fun i ->
            let name,nexti = NameGen.nextName i
            let xml = tag name
            let collector = 
                fun env -> 
                    let x = Environ.lookup (name + ".x") env
                    let y = Environ.lookup (name + ".y") env
                    let value =
                        match x,y with
                        | [Value x],[Value y] -> Some <| System.Drawing.Point(int x, int y)
                        | _ -> None
                    xml,([],Some value)
            (xml,collector),nexti

    /// <summary>
    /// Creates a &lt;br/&gt; tag
    /// </summary>
    let br = tag "br" [] nop

    /// <summary>
    /// Creates a &lt;div/&gt; tag
    /// </summary>
    let div attr = tag "div" attr

    /// <summary>
    /// Creates a &lt;span/&gt; tag
    /// </summary>
    let span attr = tag "span" attr

    /// <summary>
    /// Creates a &lt;table/&gt; tag
    /// </summary>
    let table attr = tag "table" attr

    /// <summary>
    /// Creates a &lt;tr/&gt; tag
    /// </summary>
    let tr attr = tag "tr" attr

    /// <summary>
    /// Creates a &lt;td/&gt; tag
    /// </summary>
    let td attr = tag "td" attr

    /// <summary>
    /// Creates a &lt;tbody/&gt; tag
    /// </summary>
    let tbody attr = tag "tbody" attr

    /// <summary>
    /// Creates a &lt;thead/&gt; tag
    /// </summary>
    let thead attr = tag "thead" attr

    /// <summary>
    /// Creates an &lt;iframe&gt; tag
    /// </summary>
    /// <param name="src">Iframe src</param>
    /// <param name="attr">Additional attributes</param>
    let iframe src attr = tag "iframe" (attr @ ["src",src]) nop

    /// <summary>
    /// Creates a &lt;noscript&gt; tag
    /// </summary>
    /// <param name="x">Content</param>
    let noscript x = tag "noscript" [] x

    /// <summary>
    /// Creates a &lt;script&gt; tag
    /// </summary>
    /// <param name="src">Script src</param>
    let script src = tag "script" ["type","text/javascript"; "src",src] nop

    /// <summary>
    /// Creates a &lt;fieldset&gt; tag
    /// </summary>
    /// <param name="attr">Attributes</param>
    let fieldset attr = tag "fieldset" attr

    /// <summary>
    /// Creates a &lt;legend&gt; tag
    /// </summary>
    /// <param name="attr">Attributes</param>
    let legend attr = tag "legend" attr

    /// <summary>
    /// reCaptcha settings
    /// </summary>
    type ReCaptchaSettings = {
        /// reCaptcha public key
        PublicKey: string
        /// reCaptcha private key
        PrivateKey: string

        /// <summary>
        /// Mocked result for testing. 
        /// If <c>Some x</c>, the response will not be validated against the reCaptcha service and will return <c>x</c> instead
        /// </summary>
        MockedResult: bool option
    }

    /// <summary>
    /// Creates a reCaptcha formlet
    /// </summary>
    /// <param name="settings">reCaptcha settings</param>
    /// <param name="requestIP">request IP, used for validation only</param>
    /// <seealso href="http://www.google.com/recaptcha">reCaptcha</seealso>
    let reCaptcha (settings: ReCaptchaSettings) requestIP = 
        let validate (challenge, response) =
            match settings.MockedResult with
            | Some v -> v
            | _ ->
                if isNullOrWhiteSpace response
                    then false
                    else
                        let nv = NameValueCollection.fromSeq ["privatekey",settings.PrivateKey; "remoteip",requestIP; "challenge",challenge; "response",response]
                        use http = new System.Net.WebClient()
                        let bytes = http.UploadValues("http://www.google.com/recaptcha/api/verify", nv)
                        (System.Text.Encoding.UTF8.GetString bytes).Split('\n').[0] = "true"
        
        script (sprintf "http://www.google.com/recaptcha/api/challenge?k=%s" settings.PublicKey)
        *> noscript (
            yields tuple2
            <*> iframe (sprintf "http://www.google.com/recaptcha/api/noscript?k=%s" settings.PublicKey) ["height","300"; "width","500"; "frameborder","0"] 
            *> assignedTextarea "recaptcha_challenge_field" "" ["rows","3"; "cols","40"]
            <*> assignedHidden "recaptcha_response_field" "manual_challenge"
        )
        |> satisfies (err validate (fun (_,_) -> "Invalid captcha"))
        |> map ignore

    let antiCSRF (session: IDictionary<string,obj>) : unit Formlet = 
        let eq a b = obj.Equals(a,b)
        let tokenKey = "antiCSRF_token"
        let rng = new System.Security.Cryptography.RNGCryptoServiceProvider()
        let tokenValue =
            match session.TryGetValue tokenKey with
            | true, v -> unbox v
            | _ ->
                let buffer : byte[] = Array.zeroCreate 10
                rng.GetBytes(buffer)
                let t = Convert.ToBase64String buffer
                session.Add(tokenKey, t)
                t
        hidden tokenValue
        |> satisfies (err (eq session.[tokenKey]) (fun _ -> "Invalid CSRF token"))
        |> map ignore

    let pickler (value: 'a) : 'a Formlet =
        hidden (losSerializer.Serialize value)
        |> map (losSerializer.Deserialize >> unbox)