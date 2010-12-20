module NameValueCollection

open System.Collections.Specialized

let concat a b = 
    let x = NameValueCollection()
    x.Add a
    x.Add b
    x

let toSeq (a: NameValueCollection) =
    a.AllKeys
    |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))

let toList a = toSeq a |> Seq.toList
