namespace Formlets


type xml_item = 
    | Text of string 
    | Tag of string * (string*string) list * xml_item list // tagname, attributes, children
    with
        static member getChildren =
            function
            | Tag(_,_,c) -> c
            | _ -> failwith "Expected tag, got text"
    

type 'a XmlWriter = xml_item list * 'a

/// Applicative functor that manipulates HTML as XML
module XmlWriter =
    let inline puree v : 'a XmlWriter = [],v
    //let ap (x: xml_item list,f) (y,a) = x @ y, f a
    let ap (f: ('a -> 'b) XmlWriter) (x: 'a XmlWriter) : 'b XmlWriter =
        let ff = fst f
        let sf = snd f
        let fx = fst x
        let sx = snd x
        ff @ fx, sf sx
    let inline (<*>) f x = ap f x
    let inline lift f x = puree f <*> x
    let inline lift2 f x y = puree f <*> x <*> y
    let inline plug (k: xml_item list -> xml_item list) (v: 'a XmlWriter): 'a XmlWriter = 
        k (fst v), snd v
    let inline xml (e: xml_item list) : unit XmlWriter = 
        plug (fun _ -> e) (puree ())
    let inline text s = xml [Text s]
    let inline tag name attributes (v: 'a XmlWriter) : 'a XmlWriter = 
        plug (fun x -> [Tag (name, attributes, x)]) v
    open System.Xml.Linq
    let render xml =
        let (!!) x = XName.op_Implicit x
        let xattr (name, value: string) = XAttribute(!!name, value)
        let xelem name (attributes: obj list) (children: obj list) = 
            let children = 
                match children with
                | [] -> [box (XText "")]
                | x -> x
            XElement(!!name, attributes @ children)
        let rec render' = 
            function
            | [] -> []
            | x::xs -> 
                let this = 
                    match x with
                    | Text t -> box (XText t)
                    | Tag (name, attr, children) -> box (xelem name (attr |> List.map (xattr >> box)) (render' children))
                this::(render' xs)
        XDocument(render' xml)
