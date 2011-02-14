namespace Formlets

open System.Collections.Generic
open System.Collections.Specialized
open System.Web

type InputValue =
    | Value of string
    | File of HttpPostedFileBase

type EnvDict = (string * InputValue) list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EnvDict =
    let empty = []
    let addFromSeq (l: (string*InputValue) seq) (d: EnvDict) =
        d @ Seq.toList l
    let addFromValueSeq (l: (string*string) seq) =
        l |> Seq.map (fun (k,v) -> k, Value v) |> addFromSeq
    let addFromNV (n: NameValueCollection) =
        n |> NameValueCollection.toSeq |> addFromValueSeq
    let fromNV n = addFromNV n empty
    let fromValueSeq l = addFromValueSeq l empty
    let addFromFileSeq (l: (string*HttpPostedFileBase) seq) =
        l |> Seq.map (fun (k,v) -> k, File v) |> addFromSeq
    let fromFileSeq l = addFromFileSeq l empty
    let internal requestFiles (r: HttpRequestBase) =
        r.Files.AllKeys
        |> Seq.map (fun k -> k, r.Files.[k])
    let fromFormAndFiles (r: HttpRequestBase) : EnvDict =
        let env = fromNV r.Form
        env |> addFromFileSeq (requestFiles r)


type 'a Environ = EnvDict -> 'a

/// Applicative functor that handles value lookup from submitted form
module Environ = 
    let inline puree v : 'a Environ = fun (env: EnvDict) -> v
    let inline ap (f: ('a -> 'b) Environ) (a: 'a Environ) : 'b Environ = 
        fun (env: EnvDict) -> f env (a env)
    let inline (<*>) f x = ap f x
    let inline map f x : 'b Environ = puree f <*> x
    let inline map2 f x y : 'c Environ = puree f <*> x <*> y

    let lookup (n: string) : InputValue list Environ = 
        fun (env: EnvDict) ->
            let folder acc elem = 
                let key,value = elem
                if key = n
                    then value::acc
                    else acc
            List.fold folder [] env
