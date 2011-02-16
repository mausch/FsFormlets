// Learn more about F# at http://fsharp.net
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

open System

type XhtmlFormlets() =
    let e = XhtmlElement()
    let s = e.Shortcut

    /// Adds attributes to a node
    let addAttributes = 
        let folder attr node =
            let name,value = attr
            node |> Alter.addAttribute (xName name, value)
        List.foldBack folder 

    member x.Textf fmt = Printf.ksprintf e.Text fmt

    member x.CheckBox(value, ?attributes) =
        let attributes = defaultArg attributes []
        Formlet.checkbox value attributes
   
    member x.Textarea(?value, ?attributes: (string * string) list) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        Formlet.textarea value attributes

    member x.iTextBox(value: string option, attributes: (string*string) list option, required: bool option, size: int option, maxlength: int option) =
        let value = defaultArg value ""
        let attributes = defaultArg attributes []
        let required = defaultArg required false
        let requiredAttr = if required then ["required",""] else []
        let size = match size with Some v -> ["size",v.ToString()] | _ -> []
        let maxlength = match maxlength with Some v -> ["maxlength",v.ToString()] | _ -> []
        let attributes = 
            [requiredAttr;size;maxlength]
            |> List.fold (fun s e -> s |> mergeAttr e) attributes
        let formlet = Formlet.input value attributes
        let formlet =
            if required
                then formlet |> Validate.notEmpty
                else formlet
        formlet

    member x.TextBox(?value, ?attributes: (string * string) list, ?required: bool, ?size: int, ?maxlength: int) =
        x.iTextBox(value, attributes, required, size, maxlength)

    member internal x.LabeledElement(text, f, attributes) =
        let e = XhtmlElement()
        let id = "l" + Guid.NewGuid().ToString()
        let label = e.Label ["for",id] [Node.Text text]
        let attributes = attributes |> mergeAttr ["id",id]
        label +> f attributes

    member x.LabeledTextBox(text, value, ?attributes: _ list) =
        let attributes = defaultArg attributes []
        let t (att: _ list) = x.TextBox(value, att)
        x.LabeledElement(text, t, attributes)

    member x.LabeledCheckBox(text, value, ?attributes: _ list) =
        let attributes = defaultArg attributes []
        let t att = x.CheckBox(value, att)
        x.LabeledElement(text, t, attributes)

    member private x.iNumBox(value: float option, attributes: _ list option, required: bool option, size: int option, maxlength: int option, errorMsg: (string -> string) option) =
        let value = match value with Some v -> Some <| v.ToString() | _ -> None
        let errorMsg = defaultArg errorMsg (fun _ -> "Invalid number")
        x.iTextBox(value, attributes, required, size, maxlength)
        |> mergeAttributes ["type","number"]
        |> satisfies (err (Double.TryParse >> fst) errorMsg)
        |> map float

    member x.NumBox(?value: float, ?attributes: _ list, ?required: bool, ?size: int, ?maxlength: int, ?errorMsg: string -> string) =
        x.iNumBox(value, attributes, required, size, maxlength, errorMsg)

    member x.IntBox(?value: int, ?attributes: _ list, ?required: bool, ?size: int, ?maxlength: int, ?errorMsg: string -> string) =
        let value = Option.map float value
        let errorMsg2 (i: float) =
            match errorMsg with
            | Some f -> i.ToString() |> f
            | _ -> "Invalid number"
        x.iNumBox(value, attributes, required, size, maxlength, errorMsg)
        |> satisfies (err (fun n -> Math.Truncate n = n) errorMsg2)
        |> map int

[<AutoOpen>]
module Integration2 =
    type WingBeats.Xhtml.XhtmlElement with
        member x.Formlets = XhtmlFormlets()
