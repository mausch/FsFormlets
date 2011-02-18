namespace Formlets

open System.Xml.Linq

type 'a XmlWriter = XNode list * 'a

[<AutoOpen>]
module XmlHelpers =
    /// Gets attributes of an element as a tuple list
    let getAttr (e: XElement) =
        e.Attributes() 
        |> Seq.map (fun a -> a.Name.LocalName,a.Value)
        |> Seq.toList

    /// <summary>
    /// Matches a <see cref="XElement"/>
    /// </summary>
    /// <param name="n"></param>
    let (|Tag|_|) (n: XNode) = 
        match n with
        | :? XElement as e -> Some e
        | _ -> None

    /// <summary>
    /// Matches a <see cref="XElement"/>, splitting name, attributes and children
    /// </summary>
    /// <param name="n"></param>
    let (|TagA|_|) (n : XNode) =
        match n with
        | Tag t -> 
            let name = t.Name.LocalName
            let attr = getAttr t
            let children = t.Nodes() |> Seq.toList
            Some(name,attr,children)
        | _ -> None

    /// <summary>
    /// Matches a <see cref="XText"/>
    /// </summary>
    /// <param name="n"></param>
    let (|Text|_|) (n: XNode) =
        match n with
        | :? XText as t -> Some t
        | _ -> None

    /// <summary>
    /// Matches a <see cref="XText"/>, extracting the actual text value
    /// </summary>
    /// <param name="n"></param>
    let (|TextV|_|) (n: XNode) =
        match n with
        | Text t -> Some t.Value
        | _ -> None

    /// <summary>
    /// Matches a <see cref="XComment"/>
    /// </summary>
    /// <param name="n"></param>
    let (|Comment|_|) (n: XNode) =
        match n with
        | :? XComment as c -> Some c
        | _ -> None

    /// <summary>
    /// Matches a <see cref="XComment"/>, extract the actual text value
    /// </summary>
    /// <param name="n"></param>
    let (|CommentV|_|) (n: XNode) =
        match n with
        | Comment c -> Some c.Value
        | _ -> None

/// Applicative functor that manipulates HTML as XML
module XmlWriter =
    let emptyElems = set ["area";"base";"basefont";"br";"col";"frame";"hr";"img";"input";"isindex";"link";"meta";"param"]
    let inline (!!) x = XName.op_Implicit x
    let inline xattr (name, value: string) = XAttribute(!!name, value)
    let xelem name (attributes: (string*string) list) (children: XNode list) = 
        let isEmpty = emptyElems |> Set.contains name
        let children = 
            match children,isEmpty with
            | [],false -> [(XText "") :> XObject]
            | _ -> List.map (fun x -> upcast x) children
        let attributes = List.map (fun a -> xattr a :> XObject) attributes
        XElement(!!name, attributes @ children) :> XNode

    let inline puree v : 'a XmlWriter = [],v
    //let ap (x: xml_item list,f) (y,a) = x @ y, f a
    let ap (f: ('a -> 'b) XmlWriter) (x: 'a XmlWriter) : 'b XmlWriter =
        let ff = fst f
        let sf = snd f
        let fx = fst x
        let sx = snd x
        ff @ fx, sf sx
    let inline (<*>) f x = ap f x
    let inline map f x = puree f <*> x
    let inline map2 f x y = puree f <*> x <*> y
    let inline plug (k: XNode list -> XNode list) (v: 'a XmlWriter): 'a XmlWriter = 
        k (fst v), snd v
    let inline xml (e: XNode list) : unit XmlWriter = e,()
    let inline rawXml (x: string) : unit XmlWriter = 
        let x = sprintf "<r>%s</r>" x
        let xdoc = XDocument.Parse x
        xdoc.Document.Root.Nodes() |> Seq.toList |> xml
    let inline text (s: string) = xml [XText s]
    let inline tag name attributes (v: 'a XmlWriter) : 'a XmlWriter = 
        plug (fun x -> [xelem name attributes x]) v

    let inline xnode (e: XNode) : unit XmlWriter = [e],()
    let render (e: XNode seq) : string =
        let x = XElement(XName.op_Implicit "r", e)
        let r = x.CreateReader()
        r.MoveToContent() |> ignore
        r.ReadInnerXml()

    let mergeAttr a x =
        let mergeInNode =
            function
            | TagA(name, attr, children) -> 
                let attr = attr |> mergeAttr a
                xelem name attr children
            | x -> x
        plug (List.map mergeInNode) x

    let getId =
        function
        | [TagA(_,attr,_)], _ -> List.tryFind (fun (k,_) -> k = "id") attr |> Option.map snd
        | _ -> None

    let labelFor id (text: string) = 
        xelem "label" ["for", id] [XText text]
