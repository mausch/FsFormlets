namespace Formlets

type 'a NameGen = int -> 'a * int

/// Applicative functor that generates form element names
module NameGen =
    let puree v : 'a NameGen = fun (gen: int) -> v,gen
    let ap (f: ('a -> 'b) NameGen) (a: 'a NameGen) : 'b NameGen =
        fun (gen: int) ->
            let v,gen = f gen
            let w,gen = a gen
            v w, gen
    let (<*>) f x = ap f x
    let lift f x = puree f <*> x
    let lift2 f x y = puree f <*> x <*> y
    let nextName : string NameGen = 
        fun gen -> "input_" + gen.ToString(), gen+1
    let run (c: 'a NameGen) = fst (c 0)
