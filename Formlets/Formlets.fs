namespace Formlets

(*
Formlets implementation based on http://groups.inf.ed.ac.uk/links/formlets/
TODO:
* extend to use with querystring
* change return type to Success | Fail | Invalid instead of Some | None (Error applicative)
* radio and select: accept any type as value, not just strings, map them dynamically to string and back
*)

open System
open System.Collections.Generic
open System.Collections.Specialized
open System.Web

/// Validator type.
/// Fst determines if value is valid
/// Snd builds an error message
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

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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

    let private liftXml v =
        let xml1 = XmlWriter.lift Error.puree v |> Environ.puree
        let xml2 = XmlWriter.lift (fun _ -> xml1) v
        NameGen.puree xml2

    /// Lifts a xml tree to formlet
    let xml x : unit Formlet = 
        let v = XmlWriter.xml x
        liftXml v

    open System.Xml.Linq
    let xelem (e: XElement) : unit Formlet =
        let v = XmlWriter.xelem e
        liftXml v

    /// No-operation formlet
    let nop = puree ()

    /// Lifts text to formlet
    let text s : unit Formlet = xml [Text s]

    /// <summary>
    /// Lifts a HTML tag to formlet
    /// </summary>
    /// <param name="name">Tag name</param>
    /// <param name="attributes">Element attributes</param>
    /// <param name="f">Inner formlet</param>
    let tag name attributes (f: 'a Formlet) : 'a Formlet = 
        let xtag x = XmlWriter.tag name attributes x
        let xml1 x = XmlWriter.lift id (xtag x)
        let xml2 x = Environ.lift xml1 x
        let xml3 x = XmlWriter.lift xml2 (xtag x)
        NameGen.lift xml3 f

    /// Runs a formlet.
    /// Returns a populated form with error messages and the result value
    let run (v: 'a Formlet) : EnvDict -> (xml_item list * 'a option)  = 
        NameGen.run v |> snd

    /// Renders a formlet to a xml tree
    let renderToNodes (v: _ Formlet): xml_item list = 
        NameGen.run v |> fst
    
    /// Renders a formlet to XNode
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

    /// <summary>
    /// Constructs a validator
    /// </summary>
    /// <param name="isValid">Determines if value is valid</param>
    /// <param name="errorMsg">Builds the error message</param>
    let err (isValid: 'a -> bool) (errorMsg: 'a -> string) : 'a Validator = 
        let addError value xml = 
            [
                Tag("span", ["class","errorinput"], xml)
                Tag("span", ["class","error"], [Text(errorMsg value)])
            ]
        isValid, addError

    /// <summary>
    /// Constructs a validator from a regular expression
    /// </summary>
    /// <param name="rx">Regular expression to match</param>
    /// <param name="errorMsg">Builds the error message</param>
    let errx (rx: string) (errorMsg: string -> string) : string Validator =
        let v value = System.Text.RegularExpressions.Regex(rx).IsMatch(value)
        err v errorMsg

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

    /// <summary>
    /// Creates a &lt;input type=&quot;text&quot;&gt; formlet
    /// </summary>
    /// <param name="value">Default value</param>
    /// <param name="attributes">Additional attributes</param>
    let input value attributes : string Formlet = 
        let tag name boundValue = 
            let valueAttr = getValueAttr boundValue
            [Tag("input", ["name", name] @ valueAttr @ attributes, [])]
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
            [Tag("input", ["name", name] @ valueAttr @ attributes, [])]
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
            [Tag("input", ["name",name; "type","checkbox"] @ valueAttr, [])]
        let on = if on then [Value ""] else []
        generalGeneratedElement on tag 
        |> extractOptional
        |> lift transform

    /// <summary>
    /// Creates a &lt;input type=&quot;radio&quot;&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected value</param>
    /// <param name="choices">Select options</param>
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

    /// <summary>
    /// Creates a &lt;select&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected value</param>
    /// <param name="choices">Select options</param>
    let select selected (choices: (string*string) seq): string Formlet = 
        generalGeneratedElement [] (selectTag [selected] choices [])
        |> extractString

    /// <summary>
    /// Creates a &lt;select multiple&gt; formlet
    /// </summary>
    /// <param name="selected">Initial selected values</param>
    /// <param name="choices">Select options</param>
    let selectMulti selected (choices: (string*string) seq): string list Formlet = 
        generalGeneratedElement [] (selectTag selected choices ["multiple","multiple"]) 
        |> extractStrings
    
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
            [Tag("textarea", ["name",name] @ attr, [Text content])]
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
    let submit n = tag "input" ["type","submit"; "value",n] nop

    /// <summary>
    /// Creates an &lt;input type=&quot;image&quot;&gt; tag
    /// </summary>
    /// <param name="src">Image src</param>
    let image src = tag "input" ["type","submit"; "src",src] nop

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
        |> lift ignore

    let antiCSRF (session: IDictionary<string,obj>) : unit Formlet = 
        let eq a b = obj.Equals(a,b)
        let tokenKey = "antiCSRF_token"
        use rng = new System.Security.Cryptography.RNGCryptoServiceProvider()
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
        |> lift ignore
