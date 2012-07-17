namespace Formlets

open FSharpx.Monoid

type MonoidPairApplicative<'m>(m: 'm Monoid) = 
    member this.puree a = 
        m.mempty, a
    member this.ap (xa,xb) (fa,fb) = 
        m.mappend fa xa, fb xb
    member this.map f x = 
        this.puree f |> this.ap x
    member this.lift2 f a b = 
        this.puree f |> this.ap a |> this.ap b
    member this.append v (a,b) =
        m.mappend a v, b
    member this.plug f (a,b) = 
        f a, b
        
type ListPairApplicative<'a>() =
    inherit MonoidPairApplicative<'a list>(ListMonoid<'a>())
