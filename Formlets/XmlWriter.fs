namespace Formlets


type xml_item = 
    | Text of string 
    | Tag of string * (string*string) list * xml_item list // tagname, attributes, children

type 'a XmlWriter = xml_item list * 'a

/// Applicative functor that manipulates HTML as XML
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
