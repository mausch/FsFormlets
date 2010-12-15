module NameValueCollection

open System.Collections.Specialized

let concat a b = 
    let x = NameValueCollection()
    x.Add a
    x.Add b
    x

/// Ignores duplicate keys
let toSeq (a: NameValueCollection) =
    a.AllKeys
    |> Seq.map (fun k -> k, a.[k])    

/// Ignores duplicate keys
let toList a = toSeq a |> Seq.toList
