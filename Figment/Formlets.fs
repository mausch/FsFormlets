module Figment.Formlets

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
    let puree v = [],v
        //NameValueCollection(),v
    let ap (x,f) (y,a) = 
        x @ y, f a
        //NameValueCollection.concat x y, f a
    let applicative = puree, ap
    let plug k (x,v) = k x, v
    let xml e = plug (fun _ -> e) (puree ())
    let text s = xml [Text s]
    let tag t ats v = plug (fun x -> [Tag (t, ats, x)]) v
    let run = id
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
        XDocument(xelem "html" [] (render' xml))
        

module NameGen =
    let puree v gen = v,gen
    let ap f a gen =
        let v,gen = f gen
        let w,gen = a gen
        v w, gen
    let applicative = puree, ap
    let nextName gen = "input_" + gen.ToString(), gen+1
    let run c = fst (c 0)

module Environ = 
    let puree v env = v
    let ap f a env = f env (a env)
    let applicative = puree, ap
    (*
    let lookup (n: string) (env: NameValueCollection) = 
        let v = env.[n]
        if v = null
            then failwith ("Not found : " + n)
            else v
            *)
    let rec lookup n = function
    | []                    -> failwith ("Not found : " + n)
    | (m,v)::_   when n = m -> v
    | _    ::env            -> lookup n env 
    let run = id

module Formlet =
    //  AE = Compose (XmlWriter) (Environment) 
    let ae_pure x = XmlWriter.puree (Environ.puree x)
    let ae_ap f x = 
        let (<*>) a b = XmlWriter.ap a b
        XmlWriter.puree Environ.ap <*> f <*> x
    let ae = ae_pure, ae_ap

    // Compose (NameGen) (AE)
    let puree x = NameGen.puree (ae_pure x)
    let ap f x = 
        let (<*>) a b = NameGen.ap a b
        NameGen.puree ae_ap <*> f <*> x
    let (<*>) f x = ap f x
    let applicative = puree, ap

    let refine (f, g) v = 
        let f_pure, f_ap = f
        let g_pure = fst g
        f_ap (f_pure g_pure) v
    let XmlEnv_refine v = refine (XmlWriter.applicative, Environ.applicative) v
    let xml x = NameGen.puree (XmlEnv_refine (XmlWriter.xml x))
    let text s = NameGen.puree (XmlEnv_refine (XmlWriter.text s))
    let tag t ats f = 
        let (<*>) a b = NameGen.ap a b
        NameGen.puree (XmlWriter.tag t ats) <*> f
    let run v = 
        let xml, collector = XmlWriter.run (NameGen.run v)
        xml, Environ.run collector
    let input x =
        let (<*>) a b = NameGen.ap a b
        let f = NameGen.puree (fun n -> XmlWriter.tag "input" [("name", n)] (XmlWriter.puree (Environ.lookup n))) <*> NameGen.nextName
        f x
    let render v = 
        let xml = (run >> fst) v
        XmlWriter.render xml
        