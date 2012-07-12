namespace Formlets

open System.Xml.Linq

module XmlHelpers =
    let emptyElems = set ["area"; "base"; "basefont"; "br"; "col"; "command"; "frame"; "hr"; "img"; "input"; "isindex"; "keygen"; "link"; "meta"; "param"; "source"; "track"; "wbr"]
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
