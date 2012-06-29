module TestHelpers

open Xunit
open System
open System.Xml
open System.Xml.Linq
open System.Collections.Generic
open Formlets

// DSL for XML literals, from http://fssnip.net/U
let inline (!) s = XName.Get(s)
let inline (@=) xn value = XAttribute(xn, value)
let (@?=) xn value = match value with Some s -> XAttribute(xn, s) | None -> null
type XName with 
    member xn.Item 
        with get([<ParamArray>] objs: obj[]) = 
            if objs = null then null else XElement(xn, objs)

let assertThrows<'e when 'e :> exn> f = 
    Assert.Throws<'e>(Assert.ThrowsDelegate(f)) |> ignore

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

let internal xnodeComparer = 
    { new IComparer<XNode> with
        member x.Compare(a,b) = 
            if a =. b then 0 else 1 }

let internal xnodeEqualityComparer = 
    { new IEqualityComparer<XNode> with
        member x.Equals(a,b) = a =. b
        member x.GetHashCode a = a.GetHashCode() }

let internal xnodeListComparer =
    { new IComparer<XNode list> with
        member x.Compare(a,b) = 
            let eq = a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
            if eq then 0 else 1 }

let internal xnodeListEqualityComparer = 
    { new IEqualityComparer<XNode list> with
        member x.Equals(a,b) = 
            a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
        member x.GetHashCode a = a.GetHashCode() }

type Assert with
    static member XmlEqual(x: XNode, y: XNode) = Assert.Equal(x,y, xnodeEqualityComparer)
    static member XmlEqual(x: XNode list, y: XNode list) = Assert.Equal(x,y, xnodeListEqualityComparer)
    static member XmlEqual(x: XNode, y: XNode list) = Assert.XmlEqual([x],y)
    static member XmlEqual(x: XNode list, y: XNode) = Assert.XmlEqual(x,[y])
    static member XmlEqual(x: string, y: XNode) = Assert.XmlEqual(XNode.Parse x, [y])
    static member XmlEqual(x: XNode, y: string) = Assert.XmlEqual([x], XNode.Parse y)
    static member XmlEqual(x: string, y: XNode list) = Assert.XmlEqual(XNode.Parse x, y)
    static member XmlEqual(x: XNode list, y: string) = Assert.XmlEqual(x, XNode.Parse y)
    static member XmlEqual(x: string, y: string) = Assert.XmlEqual(XNode.Parse x, XNode.Parse y)