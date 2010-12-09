module Formlets

(*
Formlets implementation based on http://groups.inf.ed.ac.uk/links/formlets/
TODO:
* extend to use with querystring
* use wing beats in syntax?
* default values for form elements
* implement all form elements (textarea, select, radio, checkbox)
*)

open System.Collections.Specialized

type xml_item = 
    | Text of string 
    | Tag of string * (string*string) list * xml_item list // tagname, attributes, children

module NameValueCollection =
    let concat a b = 
        let x = NameValueCollection()
        x.Add a
        x.Add b
        x

    let toList (a: NameValueCollection) =
        a.AllKeys
        |> Seq.map (fun k -> k, a.[k])
        |> Seq.toList

type 'a XmlWriter = xml_item list * 'a
module XmlWriter =
    let puree v : 'a XmlWriter = [],v
    //let ap (x: xml_item list,f) (y,a) = x @ y, f a
    let ap (f: ('a -> 'b) XmlWriter) (x: 'a XmlWriter) : 'b XmlWriter =
        let ff = fst f
        let sf = snd f
        let fx = fst x
        let sx = snd x
        ff @ fx, sf sx
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let plug (k: xml_item list -> xml_item list) (v: 'a XmlWriter): 'a XmlWriter = 
        k (fst v), snd v
    let xml (e: xml_item list) : unit XmlWriter = 
        plug (fun _ -> e) (puree ())
    let text s = xml [Text s]
    let tag name attributes (v: 'a XmlWriter) : 'a XmlWriter = 
        plug (fun x -> [Tag (name, attributes, x)]) v
    open System.Xml.Linq
    let render xml =
        let (!!) x = XName.op_Implicit x
        let xattr (name, value: string) = XAttribute(!!name, value)
        let xelem name (attributes: obj list) (children: obj list) = XElement(!!name, attributes @ children)
        let rec render' = 
            function
            | [] -> []
            | x::xs -> 
                let this = 
                    match x with
                    | Text t -> box (XText(t))
                    | Tag (name, attr, children) -> box (xelem name (attr |> List.map (xattr >> box)) (render' children))
                this::(render' xs)
        XDocument(render' xml)
        
type 'a NameGen = int -> 'a * int
module NameGen =
    let puree v : 'a NameGen = fun (gen: int) -> v,gen
    let ap (f: ('a -> 'b) NameGen) (a: 'a NameGen) : 'b NameGen =
        fun (gen: int) ->
            let v,gen = f gen
            let w,gen = a gen
            v w, gen
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let nextName : string NameGen = 
        fun gen -> "input_" + gen.ToString(), gen+1
    let run (c: 'a NameGen) = fst (c 0)

type 'a Environ = NameValueCollection -> 'a
module Environ = 
    let puree v : 'a Environ = fun (env: NameValueCollection) -> v
    let ap (f: ('a -> 'b) Environ) (a: 'a Environ) : 'b Environ = 
        fun (env: NameValueCollection) -> f env (a env)
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let lookup (n: string) : string Environ = 
        fun (env: NameValueCollection) -> 
            let v = env.[n]
            if v = null
                then failwith ("Not found : " + n)
                else v

type 'a Error = 'a option
module Error =
    let puree v : 'a Error = Some v
    let ap (f: ('a -> 'b) Error) (a: 'a Error) : 'b Error = 
        match f,a with
        | Some f, Some a -> Some (f a)
        | _ -> None
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let failure : 'a Error = None

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
    // EO = Compose (Environment) (Error)
    let private eo_pure x : 'a EO = Environ.puree (Error.puree x)
    let private eo_ap (f: ('a -> 'b) EO) (x: 'a EO) : 'b EO = 
        (Environ.lift2 Error.ap) f x

    // AEO = Compose (XmlWriter) (EO) 
    let private aeo_pure x : 'a AEO = XmlWriter.puree (eo_pure x)
    let private aeo_ap (f: ('a -> 'b) AEO) (x: 'a AEO) : 'b AEO = 
        (XmlWriter.lift2 eo_ap) f x

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
    let private ao_lift (f: 'a -> 'b) (x: 'a AO) : 'b AO =
        ao_ap (ao_pure f) x

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

    let (<*>) f x = ap f x
    let lift f a = puree f <*> a
    let (|>>) x f = lift f x
    let lift2 f a b = puree f <*> a <*> b
    let lift3 f a b c = puree f <*> a <*> b <*> c
    let lift4 f a b c d = puree f <*> a <*> b <*> c <*> d
    let apr x y = lift2 (fun _ z -> z) x y
    let ( *>) x y = apr x y
    let apl x y = lift2 (fun z _ -> z) x y
    let (<*) x y = apl x y
    let pair a b = lift2 (fun x y -> x,y) a b
    let ( **) a b = pair a b

    let yields = puree // friendly alias

    let private XmlEnv_refine v = XmlWriter.lift eao_pure v
    let private refineAndLift f x = NameGen.puree (XmlEnv_refine (f x))
    let xml x : unit Formlet = refineAndLift XmlWriter.xml x
    let nop = xml []
    let text s : unit Formlet = refineAndLift XmlWriter.text s
    let tag name attributes (f: 'a Formlet) : 'a Formlet = 
        let g = NameGen.lift (XmlWriter.tag name attributes)
        g f
    let submit n = tag "input" ["type","submit"; "value",n] nop
    let br = tag "br" [] nop
    let run (v: 'a Formlet) : (xml_item list) * (NameValueCollection -> (xml_item list * 'a option))  = 
        NameGen.run v
    let input : string Formlet =
        let inputTag name : string AEAO = 
            let tag = XmlWriter.tag "input" ["name", name]
            let ao value = XmlWriter.puree (Error.puree value)
            tag (XmlWriter.puree (Environ.lift ao (Environ.lookup name)))
        (NameGen.lift inputTag) NameGen.nextName
    let form hmethod haction (v: 'a Formlet) : 'a Formlet = 
        tag "form" ["method",hmethod; "action",haction] v
    let render v = 
        let xml = (run >> fst) v
        XmlWriter.render xml

    let check (validator: 'a Validator) (a: 'a AO) : 'a AO =
        let result =
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
        let validationResultToError = 
            function 
            | Pass v -> Error.puree v 
            | _ -> Error.failure
        let w = XmlWriter.lift validationResultToError result
        let errorMsg = snd validator
        match result with
        | _, Fail v -> XmlWriter.plug (errorMsg v) w
        | _ -> w

    let satisfies (validator: 'a Validator) (f: 'a Formlet) : 'a Formlet =
        nae_lift (check validator) f

    let err (isValid: 'a -> bool) (errorMsg: 'a -> string) : 'a Validator = 
        let addError value xml = 
            [
                Tag("span", ["class","errorinput"], xml)
                Tag("span", ["class","error"], [Text(errorMsg value)])
            ]
        isValid, addError
