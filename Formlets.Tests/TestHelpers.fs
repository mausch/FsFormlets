module TestHelpers

open Xunit
open System.Xml.Linq

let assertThrows<'e when 'e :> exn> f = 
    Assert.Throws<'e>(Assert.ThrowsDelegate(f)) |> ignore

let inline (=.) x y =
    // attributes must be ordered due to a bug in XNode.DeepEquals
    // see http://connect.microsoft.com/VisualStudio/feedback/details/400469/xnode-deepequals-incorrect-result
    let rec orderAttributes =
        function
        | Formlets.XmlHelpers.TagA(n,a,c) -> 
            let a = a |> Seq.sortBy fst |> Seq.toList
            let c = c |> List.map orderAttributes
            Formlets.XmlWriter.xelem n a c
        | x -> x
    XNode.DeepEquals(orderAttributes x, orderAttributes y)

let internal xnodeComparer = 
    { new System.Collections.Generic.IComparer<XNode> with
        member x.Compare(a,b) = 
            if a =. b then 0 else 1 }

let internal xnodeListComparer =
    { new System.Collections.Generic.IComparer<XNode list> with
        member x.Compare(a,b) = 
            let eq = a.Length = b.Length && List.forall2 (fun x y -> x =. y) a b
            if eq then 0 else 1 }

type Assert with
    static member XmlEqual(x: XNode, y: XNode) = Assert.Equal(x,y, xnodeComparer)
    static member XmlEqual(x: XNode list, y: XNode list) = Assert.Equal(x,y, xnodeListComparer)
