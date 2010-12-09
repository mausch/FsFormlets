namespace Formlets

open System.Collections.Specialized

type 'a Environ = NameValueCollection -> 'a
module Environ = 
    let puree v : 'a Environ = fun (env: NameValueCollection) -> v
    let ap (f: ('a -> 'b) Environ) (a: 'a Environ) : 'b Environ = 
        fun (env: NameValueCollection) -> f env (a env)
    let (<*>) f x = ap f x
    let lift f x : 'b Environ = puree f <*> x
    let lift2 f x y : 'c Environ = puree f <*> x <*> y
    let optionalLookup (n: string) : string option Environ = 
        fun (env: NameValueCollection) -> 
            let v = env.[n]
            if v = null
                then None
                else Some v
    let lookup (n: string) : string Environ =
        fun (env: NameValueCollection) ->
            match optionalLookup n env with
            | None -> failwithf "Key not found in environment: %s" n
            | Some v -> v
