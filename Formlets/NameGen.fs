namespace Formlets

type 'a NameGen = int -> 'a * int

/// Applicative functor that generates form element names
module NameGen =
    let prefix = "f"
    let inline puree v : 'a NameGen = fun (gen: int) -> v,gen
    let ap (a: 'a NameGen) (f: ('a -> 'b) NameGen) : 'b NameGen =
        fun (gen: int) ->
            let w,gen = a gen
            let v,gen = f gen
            v w, gen
    let inline (<*>) f x = ap x f
    let inline map f x = puree f <*> x
    let inline lift2 f x y = puree f <*> x <*> y
    let nextName : string NameGen = 
        fun gen -> prefix + gen.ToString(), gen+1
    let inline run (c: 'a NameGen) = fst (c 0)
