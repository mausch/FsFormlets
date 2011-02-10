namespace Formlets

(*
Formlets implementation based on http://groups.inf.ed.ac.uk/links/formlets/
TODO:
* extend to use with querystring
* change return type to Success | Fail | Invalid instead of Some | None (Error applicative)
*)

open System
open System.Collections.Generic
open System.Collections.Specialized
open System.Xml.Linq
open System.Web

/// Validator type.
/// Fst determines if value is valid
/// Snd builds an error message
type 'a Validator = ('a -> bool) * ('a -> XNode list -> XNode list) * ('a -> string list)

type 'a ValidationResult =
    | Pass of 'a
    | Fail of 'a
    | Dead

type private 'a LO = 'a Error ErrorList
type private 'a ELO = 'a LO Environ
type private 'a AELO = 'a ELO XmlWriter
type private 'a AE = 'a Environ XmlWriter
type private 'a AO = 'a Error XmlWriter
type private 'a NAE = 'a AE NameGen
type private 'a EAO = 'a AO Environ
type private 'a ALO = 'a LO XmlWriter
type private 'a EALO = 'a ALO Environ
type private 'a AEALO = 'a EALO XmlWriter
type 'a Formlet = 'a Error ErrorList XmlWriter Environ XmlWriter NameGen

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Formlet =

    let inline private ae_pure x : 'a AE = XmlWriter.puree (Environ.puree x)
    let inline private ae_ap (f: ('a -> 'b) AE) (x: 'a AE) : 'b AE =
        (XmlWriter.map2 Environ.ap) f x

    let inline private nae_pure x : 'a NAE = NameGen.puree (ae_pure x)
    let inline private nae_ap (f: ('a -> 'b) NAE) (x: 'a NAE) : 'b NAE =
        (NameGen.map2 ae_ap) f x
    let inline private nae_map (f: 'a -> 'b) (x: 'a NAE) : 'b NAE = 
        nae_ap (nae_pure f) x

    let inline private ao_pure x : 'a AO = XmlWriter.puree (Error.puree x)
    let inline private ao_ap (f: ('a -> 'b) AO) (x: 'a AO) : 'b AO = 
        (XmlWriter.map2 Error.ap) f x

    let inline private eao_pure x : 'a EAO = Environ.puree (ao_pure x)
    let inline private eao_ap (f: ('a -> 'b) EAO) (x: 'a EAO) : 'b EAO =
        (Environ.map2 ao_ap) f x

    let inline private lo_pure x : 'a LO = ErrorList.puree (Error.puree x)
    let inline private lo_ap (f: ('a -> 'b) LO) (x: 'a LO) : 'b LO =
        (ErrorList.map2 Error.ap) f x
    let inline private lo_map (f: 'a -> 'b) (x: 'a LO) : 'b LO =
        lo_ap (lo_pure f) x

    let inline private alo_pure x : 'a ALO = XmlWriter.puree (lo_pure x)
    let inline private alo_ap (f: ('a -> 'b) ALO) (x: 'a ALO) : 'b ALO =
        (XmlWriter.map2 lo_ap) f x

    let inline private ealo_pure x : 'a EALO = Environ.puree (alo_pure x)
    let inline private ealo_ap (f: ('a -> 'b) EALO) (x: 'a EALO) : 'b EALO =
        (Environ.map2 alo_ap) f x

    let inline private aealo_pure x: 'a AEALO = XmlWriter.puree (ealo_pure x)
    let inline private aealo_ap (f: ('a -> 'b) AEALO) (x: 'a AEALO) : 'b AEALO =
        (XmlWriter.map2 ealo_ap) f x

    let puree x : 'a Formlet = NameGen.puree (aealo_pure x)
    let ap (f: ('a -> 'b) Formlet) (x: 'a Formlet) : 'b Formlet = 
        (NameGen.map2 aealo_ap) f x

    let inline (<*>) f x = ap f x
    let inline map f a = puree f <*> a

    /// Convenience 'map' with flipped parameters
    let inline (|>>) x f = map f x
    let inline map2 f a b = puree f <*> a <*> b
    let inline map3 f a b c = puree f <*> a <*> b <*> c
    let inline map4 f a b c d = puree f <*> a <*> b <*> c <*> d

    /// Sequence actions, discarding the value of the first argument.
    let inline apr x y = map2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = apr x y
    /// Sequence actions, discarding the value of the second argument.
    let inline apl x y = map2 (fun z _ -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline (<*) x y = apl x y
    let inline pair a b = map2 (fun x y -> x,y) a b
    let inline ( **) a b = pair a b

    let inline yields x = puree x // friendly alias

    let private mapXml (v: 'a XmlWriter) : 'a Formlet =
        let xml1 = XmlWriter.map (Error.puree >> ErrorList.puree) v |> Environ.puree
        let xml2 = XmlWriter.map (fun _ -> xml1) v
        NameGen.puree xml2

    /// maps a xml forest to formlet
    let xml x : unit Formlet = 
        XmlWriter.xml x |> mapXml

    /// Parses a raw xml forest to formlet
    let rawXml x : unit Formlet =
        XmlWriter.rawXml x |> mapXml

    /// maps a xml tree to formlet
    let inline xnode (e: XNode) : unit Formlet = xml [e]

    /// No-operation formlet
    let nop = puree ()

    /// maps text to formlet
    let inline text (s: string) : unit Formlet = xml [XText s]

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
    let inline run (v: 'a Formlet) : EnvDict -> (XNode list * string list * 'a option) = 
        (NameGen.run v |> snd) >> (fun (a,(b,c)) -> a,b,c)

    let (|Success|Failure|) a =
        match a with
        | _,_,Some v -> Success v
        | errorForm,errorMsgs,None -> Failure(errorForm, errorMsgs)
    
    /// Renders a formlet to XNode
    let inline renderToXml (v: _ Formlet) = 
        NameGen.run v |> fst |> XmlWriter.wrap

    /// Renders a formlet to string
    let inline render (v: _ Formlet) = 
        let x = renderToXml v
        x.ToString()

    // Validation functions

    let private check (validator: 'a Validator) (a: 'a ALO) : 'a ALO =
        let result: 'a ValidationResult XmlWriter =
            let errorToValidationResult (o: 'a LO) =
                let pred,_,_ = validator
                let check' p v =
                    if p v
                        then Pass v
                        else Fail v
                let mappedCheck = lo_map (check' pred)
                match mappedCheck o with
                | _,Some v -> v
                | _ -> Dead
            XmlWriter.map errorToValidationResult a
        let validationResultToError: 'b ValidationResult -> 'b LO = 
            function 
            | Pass v -> lo_pure v
            | _ -> Error.failure |> ErrorList.puree
        let w = XmlWriter.map validationResultToError result
        let _,errorXml,errorMsg = validator
        match result with
        | x, Fail v -> 
            let w =
                let append a = ErrorList.append (errorMsg v) a
                XmlWriter.map append w
            XmlWriter.plug (errorXml v) w
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
                    XmlWriter.xelem "span" ["class","errorinput"] xml
                    XmlWriter.xelem "span" ["class","error"] [XText(errorMsg value)]
                ]
            List.map (fun e -> e :> XNode) elems
        isValid, addError, errorMsg >> List.singleton

    /// <summary>
    /// Constructs a validator from a regular expression
    /// </summary>
    /// <param name="rx">Regular expression to match</param>
    /// <param name="errorMsg">Builds the error message</param>
    let errx (rx: string) (errorMsg: string -> string) : string Validator =
        let v value = System.Text.RegularExpressions.Regex(rx).IsMatch(value)
        err v errorMsg

    // Generic HTML functions

    let generalElement nameGen defaultValue (tag: string -> InputValue list -> XNode list): InputValue list Formlet =
        let t name = 
            let xml = tag name
            let ealo = 
                fun env -> 
                    let value = Environ.lookup name env
                    let xml = xml value
                    xml,([],Some value)
            XmlWriter.plug (fun _ -> xml defaultValue) (XmlWriter.puree ealo)
        NameGen.map t nameGen
            
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
        map extractOptionString f

    let extractString (f: InputValue list Formlet) : string Formlet =
        extractOptional f |> map Option.get

    let extractStrings (f: InputValue list Formlet) : string list Formlet =
        let extractOne =
            function
            | Value v -> v
            | _ -> failwith "Unexpected file"
        map (List.map extractOne) f

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
            [XmlWriter.xelem "input" (["name", name] @ valueAttr @ attributes) []]
        generalGeneratedElement [Value defaultValue] tag |> extractOptional

    // Concrete HTML functions

    /// <summary>
    /// Creates a &lt;input type=&quot;text&quot;&gt; formlet
    /// </summary>
    /// <param name="value">Default value</param>
    /// <param name="attributes">Additional attributes</param>
    let input value attributes : string Formlet = 
        let tag name boundValue = 
            let valueAttr = getValueAttr boundValue
            [XmlWriter.xelem "input" (["name", name] @ valueAttr @ attributes) []]
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
            [XmlWriter.xelem "input" (["name", name] @ valueAttr @ attributes) []]
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
            [XmlWriter.xelem "input" (["name",name; "type","checkbox"] @ valueAttr) []]
        let on = if on then [Value ""] else []
        generalGeneratedElement on tag 
        |> extractOptional
        |> map transform

    /// <summary>
    /// Creates a &lt;input type=&quot;radio&quot;&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected value</param>
    /// <param name="choices">Select options</param>
    let radio selected (choices: (string*string) seq): string Formlet =
        let makeLabel id (text: string) = 
            XmlWriter.xelem "label" ["for", id] [XText text]
        let makeRadio name value id selected = 
            let on = if selected then ["checked","checked"] else []
            XmlWriter.xelem "input" (["type","radio"; "name",name; "id",id; "value",value] @ on) []
        let tag name boundValue = 
            let selectedValue = extractOptionString boundValue |> Option.get
            choices 
            |> Seq.index
            |> Seq.map (fun (i,(value,label)) -> name, value, label, name + "_" + i.ToString(), selectedValue = value)
            |> Seq.collect (fun (name,value,label,id,selected) -> [makeRadio name value id selected; makeLabel id label])
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
        XmlWriter.xelem "option" (["value",value] @ on) [XText text]
    let internal makeSelect name attr options = 
        XmlWriter.xelem "select" (["name",name] @ attr) options
    let internal selectTag selected choices attr name boundValue =
        // TODO use boundValue
        [choices |> Seq.map (makeOption selected) |> Seq.toList |> makeSelect name attr]

    /// <summary>
    /// Creates a &lt;select&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected value</param>
    /// <param name="choices">Select options</param>
    let select selected (choices: (string*string) seq): string Formlet = 
        let tag = selectTag [selected] choices []
        generalGeneratedElement [] tag
        |> extractString

    let selectA (selected: 'a) (choices: ('a * string) seq) =
        let mappedChoices, mapHashToValue = buildHashMap choices
        select (hashs selected) mappedChoices
        |> map mapHashToValue

    /// <summary>
    /// Creates a &lt;select multiple&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected values</param>
    /// <param name="choices">Select options</param>
    let selectMulti selected (choices: (string*string) seq): string list Formlet = 
        let tag = selectTag selected choices ["multiple","multiple"]
        generalGeneratedElement [] tag
        |> extractStrings

    let selectMultiA (selected: 'a seq) (choices: ('a * string) seq) : 'a list Formlet =
        let mappedChoices, mapHashToValue = buildHashMap choices
        selectMulti (Seq.map hashs selected) mappedChoices
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
            [XmlWriter.xelem "textarea" (["name",name] @ attr) [XText content]]
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
            [XmlWriter.xelem "input" (["type", "file"; "name", name] @ attr) []]
        let fileOnly =
            function
            | Some (File f) -> Some f
            | None -> None
            | _ -> failwith "File expected, got value instead"
        let r = generalGeneratedElement [] tag
        map (Seq.nth 0 >> Some >> fileOnly) r

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
    let submit n attr = tag "input" (["type","submit"; "value",n] @ attr) nop

    /// <summary>
    /// Creates an &lt;input type=&quot;image&quot;&gt; tag
    /// </summary>
    /// <param name="src">Image src</param>
    /// <param name="attr">Additional attributes</param>
    let image src attr = tag "input" (["type","submit"; "src",src] @ attr) nop

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
                let nv = NameValueCollection.fromSeq ["privatekey",settings.PrivateKey; "remoteip",requestIP; "challenge",challenge; "response",response]
                use http = new System.Net.WebClient()
                let bytes = http.UploadValues("http://www.google.com/recaptcha/api/verify", nv)
                (System.Text.Encoding.UTF8.GetString bytes).Split('\n').[0] = "true"
        
        script (sprintf "http://www.google.com/recaptcha/api/challenge?k=%s" settings.PublicKey)
        *> noscript (
            yields t2
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
