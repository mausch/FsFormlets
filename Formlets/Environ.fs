namespace Formlets

open System.Collections.Specialized

type 'a Environ = NameValueCollection -> 'a
module Environ = 
    let puree v : 'a Environ = fun (env: NameValueCollection) -> v
    let ap (f: ('a -> 'b) Environ) (a: 'a Environ) : 'b Environ = 
        fun (env: NameValueCollection) -> f env (a env)
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let lookup (n: string) : string Environ = 
        fun (env: NameValueCollection) -> 
            let v = env.[n]
            if v = null
                then failwith ("Not found : " + n)
                else v
