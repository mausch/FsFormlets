module NameValueCollection

open System.Collections.Specialized

let concat a b = 
    let x = NameValueCollection()
    x.Add a
    x.Add b
    x

let toList (a: NameValueCollection) =
    a.AllKeys
    |> Seq.map (fun k -> k, a.[k])
    |> Seq.toList
