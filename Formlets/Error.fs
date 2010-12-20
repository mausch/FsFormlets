namespace Formlets

type 'a Error = 'a option

/// Option as applicative functor
module Error =
    let puree v : 'a Error = Some v
    let ap (f: ('a -> 'b) Error) (a: 'a Error) : 'b Error = 
        match f,a with
        | Some f, Some a -> Some (f a)
        | _ -> None
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let failure : 'a Error = None
