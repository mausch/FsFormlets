namespace Formlets

(*
Formlets implementation based on http://groups.inf.ed.ac.uk/links/formlets/
TODO:
* extend to use with querystring
* use wing beats in syntax?
* default values for form elements
* file input
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
    let run (v: 'a Formlet) : (xml_item list) * (EnvDict -> (xml_item list * 'a option))  = 
        NameGen.run v
    let private generalElement lookup (tag: string -> xml_item list): 'a Formlet =
        let t name : 'a AEAO = 
            let xml = tag name
            XmlWriter.plug (fun _ -> xml) (XmlWriter.puree (Environ.lift ao_pure (lookup name)))
        (NameGen.lift t) NameGen.nextName 
    let private generalStrictElement = generalElement Environ.lookup
    let private generalOptionalElement = generalElement Environ.optionalLookup
    let private generalStrictNonFileElement = generalElement Environ.lookupNonFile
    let private generalOptionalNonFileElement = generalElement Environ.optionalLookupNonFile
    let private optionalInput attributes: string option Formlet =
        let tag name = [Tag("input", ["name", name] @ attributes, [])]
        generalOptionalNonFileElement tag
    let input attributes : string Formlet = 
        let tag name = [Tag("input", ["name", name] @ attributes, [])]
        generalStrictNonFileElement tag
    let password : string Formlet = 
        input ["type","password"]
    let hidden : string Formlet = 
        input ["type","hidden"]
    let checkbox : bool Formlet =
        let transform = 
            function
            | None -> false
            | Some _ -> true
        lift transform (optionalInput ["type","checkbox"])
    let radio (choices: (string*string) seq): string Formlet =
        let makeLabel id text = 
            Tag("label", ["for", id], [Text text])
        let makeRadio name value id = 
            Tag("input", ["type","radio"; "name",name; "id",id; "value",value], [])
        let tag name = 
            choices 
            |> Seq.zip {1..Int32.MaxValue} 
            |> Seq.map (fun (i,(value,label)) -> name,value,label,name + "_" + i.ToString())
            |> Seq.collect (fun (name,value,label,id) -> [makeRadio name value id; makeLabel id label])
            |> Seq.toList
        generalStrictNonFileElement tag
    let select (choices: (string*string) seq): string Formlet = 
        let makeOption (value,text) = 
            Tag("option", ["value",value], [Text text])
        let makeSelect name options = 
            Tag("select", ["name",name], options)
        let tag name =
            [choices |> Seq.map makeOption |> Seq.toList |> makeSelect name]
        generalStrictNonFileElement tag
    let textarea (rows: int option) (cols: int option) : string Formlet = 
        let attributes = 
            let rows = match rows with Some r -> ["rows",r.ToString()] | _ -> []
            let cols = match cols with Some r -> ["cols",r.ToString()] | _ -> []
            rows @ cols
        let tag name = 
            [Tag("textarea", ["name", name] @ attributes, [])]
        generalStrictNonFileElement tag
    let file : HttpPostedFileBase option Formlet = 
        let tag name = 
            [Tag("input", ["type", "file"; "name", name], [])]
        let fileOnly =
            function
            | Some (File f) -> Some f
            | None -> None
            | _ -> failwith "File expected, got value instead"
        let r = generalOptionalElement tag
        lift fileOnly r
            
    let form hmethod haction attributes (v: 'a Formlet) : 'a Formlet = 
        tag "form" (["method",hmethod; "action",haction] @ attributes) v
    let render v = 
        let xml = (run >> fst) v
        XmlWriter.render xml

    let private check (validator: 'a Validator) (a: 'a AO) : 'a AO =
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
