namespace WingBeats.Formlets

open WingBeats
open WingBeats.Xml
open WingBeats.Xhtml
open Formlets

module Helpers = 
    // copied from WingBeats.Xhtml.Shortcuts
    let xName name = { Name = name; NS = {Prefix = ""; Uri = "http://www.w3.org/1999/xhtml"} }

    let xAttr (name,value) = xName name, value
    let xAttrs = List.map xAttr
    let exAttr (name: WingBeats.Xml.XName, value) = name.Name, value
    let exAttrs = List.map exAttr

    open System.Xml.Linq

    let rec renderXNodeToWingBeats =
        function
        | XmlHelpers.TextV t -> Node.Text t
        | XmlHelpers.TagA(name, attr, children) -> 
            if XmlWriter.emptyElems |> Set.contains name
                then SelfClosingNode(xName name, xAttrs attr)
                else TagPairNode(xName name, xAttrs attr, children |> List.map renderXNodeToWingBeats)
        | e -> failwithf "Unsupported element %A" e        

    let wbNode name attr children = TagPairNode(xName name, xAttrs attr, children)

    let rec renderWingBeatsNodeToXNode =
        function
        | DocType _ -> failwith "Formlets can't handle DocType"
        | TagPairNode(name, attr, children) -> 
            let name = name.Name
            let attr = exAttrs attr
            let children = children |> Seq.map renderWingBeatsNodeToXNode |> Seq.toList
            XmlWriter.xelem name attr children
        | SelfClosingNode(name, attr) -> 
            let name = name.Name
            let attr = exAttrs attr
            XmlWriter.xelem name attr []
        | Node.Text t -> upcast XText t
        | LiteralText t -> upcast XText t // probably wrong
        | Placeholder i -> failwithf "Don't know what a placeholder %d is" i
        | NoNode -> upcast XText ""

open Helpers

[<AutoOpen>]
module Integration =
    let e = XhtmlElement()

    /// Lifts a WingBeats node into a formlet
    let (<+) (a: 'a Formlet) (b: Node): 'a Formlet = 
        [renderWingBeatsNodeToXNode b] |> xml |> apl a

    /// Lifts a WingBeats node into a formlet
    let (+>) (b: Node) (a: 'a Formlet) : 'a Formlet = 
        let uf = [renderWingBeatsNodeToXNode b] |> xml
        apr uf a

    type XhtmlShortcut with
        member x.Label forId text =
            e.Label ["for",forId] [Node.Text text]
        member x.Form httpMethod action (children: #seq<Node>) =
            e.Form ["action",action; "method",httpMethod] children
        member x.FormGet url (children: #seq<Node>) = x.Form "get" url children
        member x.FormPost url (children: #seq<Node>) = x.Form "post" url children
        member x.Submit text = e.Input ["type","submit"; "value",text]

    let inline (!+) x = List.map renderXNodeToWingBeats x

[<AutoOpen>]
module Integration2 =
    type WingBeats.Xhtml.XhtmlElement with
        member x.Formlets = FormElements Validate.Default
        member x.Formlets v = FormElements v