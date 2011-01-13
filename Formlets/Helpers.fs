namespace Formlets

module NameValueCollection =
    open System.Collections.Specialized

    let concat a b = 
        let x = NameValueCollection()
        x.Add a
        x.Add b
        x

    let add (x: NameValueCollection) (a,b) = x.Add(a,b)

    let toSeq (a: NameValueCollection) =
        a.AllKeys
        |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))

    let toList a = toSeq a |> Seq.toList

    let fromSeq l =
        let x = NameValueCollection()
        Seq.iter (add x) l
        x

module Seq =
    let index a = Seq.zip (Seq.initInfinite id) a
    let tryFindWithIndex pred l =
        l |> index |> Seq.tryFind (fun (_,v) -> pred v)

module List =
    let skip i = Seq.skip i >> Seq.toList
    let take i = Seq.take i >> Seq.toList
    let sub startIndex count = Seq.skip startIndex >> Seq.take count >> Seq.toList
    // Replaces an item in a list. Probably horribly inefficient
    let replaceAt item i l =
        match i with
        | x when x < 0 -> failwith "Out of bounds"
        | x when x >= List.length l -> failwith "Out of bounds"
        | 0 -> item::List.tail l
        | x -> (take x l) @ (item::(skip (x+1) l))

[<AutoOpen>]
module Helpers = 
    /// Builds a pair (2-tuple)
    let t2 a b = a,b

    /// Builds a triple (3-tuple)
    let t3 a b c = a,b,c

    /// Builds a 4-tuple
    let t4 a b c d = a,b,c,d

    /// Builds a 5-tuple
    let t5 a b c d e = a,b,c,d,e

    /// Builds a 6-tuple
    let t6 a b c d e f = a,b,c,d,e,f

    /// Builds a 7-tuple
    let t7 a b c d e f g = a,b,c,d,e,f,g

    /// Builds a 8-tuple
    let t8 a b c d e f g h = a,b,c,d,e,f,g,h

    let appendToSameKey key sep value attr =
        match Seq.tryFindWithIndex (fun (k,_) -> k = key) attr with
        | Some (i,(k,v)) -> List.replaceAt (k,v + sep + value) i attr
        | _ -> (key,value)::attr

    let addClass = appendToSameKey "class" " "

    let addStyle = appendToSameKey "style" ";"

    let addOrOverwrite kv attr = 
        let key,value = kv
        match Seq.tryFindWithIndex (fun (k,_) -> k = key) attr with
        | Some (i,(k,v)) -> List.replaceAt (k,value) i attr
        | _ -> (key,value)::attr

    let mergeAttr a1 a2 =
        let folder r a =
            match a with
            | "class",v -> addClass v r
            | "style",v -> addStyle v r
            | _ -> addOrOverwrite a r
        Seq.fold folder a2 a1