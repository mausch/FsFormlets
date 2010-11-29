module Figment.Formlets

(*
Formlets implementation based on http://groups.inf.ed.ac.uk/links/formlets/
TODO:
* validation
* use wing beats in syntax
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

module XmlWriter =
    let puree v = ([]: xml_item list),v
    let ap (x: xml_item list,f) (y,a) = x @ y, f a
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let plug k (x,v) = k x, v
    let xml e = plug (fun _ -> e) (puree ())
    let text s = xml [Text s]
    let tag name attributes v = plug (fun x -> [Tag (name, attributes, x)]) v
    open System.Xml.Linq
    let render hmethod action xml =
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
        XDocument(xelem "form" [xattr("action",action); xattr("method",hmethod)] (render' xml))
        

module NameGen =
    let puree v = fun (gen: int) -> v,gen
    let ap f a =
        fun (gen: int) ->
            let v,gen = f gen
            let w,gen = a gen
            v w, gen
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let nextName gen = "input_" + gen.ToString(), gen+1
    let run c = fst (c 0)

module Environ = 
    let puree v = fun (env: NameValueCollection) -> v
    let ap f a = fun (env: NameValueCollection) -> f env (a env)
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let lookup (n: string) (env: NameValueCollection) = 
        let v = env.[n]
        if v = null
            then failwith ("Not found : " + n)
            else v

type 'a Formlet = int -> (xml_item list * (NameValueCollection -> 'a)) * int

module Formlet =
    //  AE = Compose (XmlWriter) (Environment) 
    let private ae_pure x = XmlWriter.puree (Environ.puree x)
    let private ae_ap f = (XmlWriter.lift2 Environ.ap) f

    // Compose (NameGen) (AE)
    let puree x : 'a Formlet = NameGen.puree (ae_pure x)
    let ap (f: ('a -> 'b) Formlet) (x: 'a Formlet) : 'b Formlet = 
        let liftedAp = NameGen.lift2 ae_ap
        liftedAp f x

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

    let private XmlEnv_refine v = XmlWriter.ap (XmlWriter.puree Environ.puree) v
    let private refineAndLift f x = NameGen.puree (XmlEnv_refine (f x))
    let xml x : unit Formlet = refineAndLift XmlWriter.xml x
    let text s : unit Formlet = refineAndLift XmlWriter.text s
    let tag name attributes (f: 'a Formlet) : 'a Formlet = 
        let g = NameGen.lift (XmlWriter.tag name attributes)
        g f
    let run (v: 'a Formlet) = NameGen.run v
    let input : string Formlet =
        fun x -> 
            let inputTag name = XmlWriter.tag "input" [("name", name)] (XmlWriter.puree (Environ.lookup name))
            let f = (NameGen.lift inputTag) NameGen.nextName
            f x
    let render hmethod action v = 
        let xml = (run >> fst) v
        XmlWriter.render hmethod action xml
        