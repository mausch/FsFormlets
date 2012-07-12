namespace Formlets

open System.Xml.Linq

type 'a XmlWriter = XNode list * 'a

/// Applicative functor that manipulates HTML as XML
module XmlWriter =
    open Formlets.Helpers
    open XmlHelpers

    let applicative = ListPairApplicative<XNode>()

    let inline xml (e: XNode list) : unit XmlWriter = e,()
    let inline parseRawXml x = XNode.Parse x
    let rawXml = parseRawXml >> xml
    let inline text (s: string) = xml [XText s]
    let inline tag name attributes (v: 'a XmlWriter) : 'a XmlWriter = 
        applicative.plug (fun x -> [xelem name attributes x]) v

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
                let attr = attr |> Helpers.mergeAttr a
                xelem name attr children
            | x -> x
        applicative.plug (List.map mergeInNode) x

    let getId =
        function
        | [TagA(_,attr,_)], _ -> List.tryFind (fun (k,_) -> k = "id") attr |> Option.map snd
        | _ -> None

    let labelFor id (text: string) = 
        xelem "label" ["for", id] [XText text]

    let labelRawFor id (xml: string) =
        xelem "label" ["for", id] (parseRawXml xml)