namespace Formlets.Tests

module TestHelpers =
    open System
    open System.Xml
    open System.Xml.Linq
    open System.Collections.Generic
    open Formlets
    open Fuchu

    // DSL for XML literals, from http://fssnip.net/U
    let inline (!) s = XName.Get(s)
    let inline (@=) xn value = XAttribute(xn, value)
    let (@?=) xn value = match value with Some s -> XAttribute(xn, s) | None -> null
    type XName with 
        member xn.Item 
            with get([<ParamArray>] objs: obj[]) = 
                if objs = null then null else XElement(xn, objs)

    let inline (=.) x y =
        // attributes must be ordered due to a bug in XNode.DeepEquals
        // see http://connect.microsoft.com/VisualStudio/feedback/details/400469/xnode-deepequals-incorrect-result
        let rec orderAttributes =
            function
            | XmlHelpers.TagA(name, attr, children) -> 
                let attr = attr |> List.sortBy fst
                let children = children |> List.map orderAttributes
                XmlHelpers.xelem name attr children
            | x -> x
        XNode.DeepEquals(orderAttributes x, orderAttributes y)

    let xnodeEqualityComparer = 
        { new IEqualityComparer<XNode> with
            member x.Equals(a,b) = a =. b
            member x.GetHashCode a = a.GetHashCode() }

    let xnodeListEqualityComparer = 
        { new IEqualityComparer<XNode list> with
            member x.Equals(a,b) = 
                a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
            member x.GetHashCode a = a.GetHashCode() }

    open Formlets.Helpers

    type Assert =
        static member inline ListExists(element, list) =
            if not (List.exists ((=) element) list)
                then failtestf "Expected element not found: %A\nActual: %A" element list
        static member inline XmlEqual(x: XNode, y: XNode) = 
            if not (xnodeEqualityComparer.Equals(x,y))
                then failtestf "Expected: %A\nActual: %A" x y
        static member inline XmlEqual(x: XNode list, y: XNode list) =
            if not (xnodeListEqualityComparer.Equals(x,y))
                then failtestf "Expected: %A\nActual: %A" x y
        static member inline XmlEqual(x: XNode, y: XNode list) = Assert.XmlEqual([x],y)
        static member inline XmlEqual(x: XNode list, y: XNode) = Assert.XmlEqual(x,[y])
        static member inline XmlEqual(x: string, y: XNode) = Assert.XmlEqual(XNode.Parse x, [y])
        static member inline XmlEqual(x: XNode, y: string) = Assert.XmlEqual([x], XNode.Parse y)
        static member inline XmlEqual(x: string, y: XNode list) = Assert.XmlEqual(XNode.Parse x, y)
        static member inline XmlEqual(x: XNode list, y: string) = Assert.XmlEqual(x, XNode.Parse y)
        static member inline XmlEqual(x: string, y: string) = Assert.XmlEqual(XNode.Parse x, XNode.Parse y)

