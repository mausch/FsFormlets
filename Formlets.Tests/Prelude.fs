namespace Formlets.Tests

[<AutoOpen>]
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

    let xnodeComparer = 
        { new IComparer<XNode> with
            member x.Compare(a,b) = 
                if a =. b then 0 else 1 }

    let xnodeEqualityComparer = 
        { new IEqualityComparer<XNode> with
            member x.Equals(a,b) = a =. b
            member x.GetHashCode a = a.GetHashCode() }

    let xnodeListComparer =
        { new IComparer<XNode list> with
            member x.Compare(a,b) = 
                let eq = a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
                if eq then 0 else 1 }

    let xnodeListEqualityComparer = 
        { new IEqualityComparer<XNode list> with
            member x.Equals(a,b) = 
                a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
            member x.GetHashCode a = a.GetHashCode() }

    let inline assertEqual msg expected actual =
        if expected <> actual
            then failtestf "%s\nExpected: %A\nActual: %A" msg expected actual

    let inline assertNone msg = 
        function
        | Some x -> failtestf "Expected None, Actual: Some (%A)" x
        | _ -> ()

    let inline assertContains expected (actual: string) = 
        if not (actual.Contains expected)
            then failtestf "Expected string containing: %s\nActual: %s" expected actual

    let inline assertRaise (ex: Type) f =
        try
            f()
            failtestf "Expected exception '%s' but no exception was raised" ex.FullName
        with e ->
            if e.GetType() <> ex
                then failtestf "Expected exception '%s' but raised:\n%A" ex.FullName e

    let inline assertListExists element list =
        if not (List.exists ((=) element) list)
            then failtestf "Expected element not found: %A\nActual: %A" element list

    type Assert =
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

