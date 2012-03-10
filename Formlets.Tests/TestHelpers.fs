module TestHelpers

open Xunit
open System.Xml.Linq
open System.Collections.Generic
open Formlets

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

let internal xnodeListComparer =
    { new IComparer<XNode list> with
        member x.Compare(a,b) = 
            let eq = a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
            if eq then 0 else 1 }

type Assert with
    static member XmlEqual(x: XNode, y: XNode) = Assert.Equal(x,y, xnodeComparer)
    static member XmlEqual(x: XNode list, y: XNode list) = Assert.Equal(x,y, xnodeListComparer)
    static member XmlEqual(x: XNode, y: XNode list) = Assert.XmlEqual([x],y)
    static member XmlEqual(x: XNode list, y: XNode) = Assert.XmlEqual(x,[y])
    static member XmlEqual(x: string, y: XNode) = Assert.XmlEqual(XNode.Parse x, [y])
    static member XmlEqual(x: XNode, y: string) = Assert.XmlEqual([x], XNode.Parse y)
    static member XmlEqual(x: string, y: XNode list) = Assert.XmlEqual(XNode.Parse x, y)
    static member XmlEqual(x: XNode list, y: string) = Assert.XmlEqual(x, XNode.Parse y)
    static member XmlEqual(x: string, y: string) = Assert.XmlEqual(XNode.Parse x, XNode.Parse y)