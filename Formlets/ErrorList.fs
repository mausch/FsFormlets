namespace Formlets

type 'a ErrorList = string list * 'a

module ErrorList =
    let inline puree v = [],v
    let ap (f: ('a -> 'b) ErrorList) (x: 'a ErrorList) : 'b ErrorList =
        let ff = fst f
        let sf = snd f
        let fx = fst x
        let sx = snd x
        ff @ fx, sf sx
    let inline (<*>) f x = ap f x
    let inline map f x = puree f <*> x
    let inline map2 f x y = puree f <*> x <*> y
    let inline append v (e: 'a ErrorList): 'a ErrorList = (fst e) @ v, snd e
