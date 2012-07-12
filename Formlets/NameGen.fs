namespace Formlets

open FSharpx

type 'a NameGen = State.State<'a, int>

/// Applicative functor that generates form element names
module NameGen =
    let prefix = "f"
    let inline puree v : 'a NameGen = State.returnM v
    let inline ap (x: _ NameGen) (f: _ NameGen) : _ NameGen = State.ap x f
    let inline (<*>) f x = ap x f
    let inline map f x = puree f <*> x
    let inline lift2 f x y = puree f <*> x <*> y
    let nextName : string NameGen = 
        fun gen -> prefix + gen.ToString(), gen+1
    let inline run (c: _ NameGen) = State.eval c 0
