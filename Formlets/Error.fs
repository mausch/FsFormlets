namespace Formlets

type 'a Error = 'a option

/// Option as applicative functor
module Error =
    let inline puree v : 'a Error = Some v
    let inline ap (f: ('a -> 'b) Error) (a: 'a Error) : 'b Error = 
        // alternative definition
        // Option.bind (fun f' -> Option.bind (fun a' -> Option.Some (f' a')) a) f
        match f,a with
        | Some f, Some a -> Some (f a)
        | _ -> None
    let inline (<*>) f x = ap f x
    let inline lift f x = puree f <*> x
    let inline lift2 f x y = puree f <*> x <*> y
    let failure : 'a Error = None
