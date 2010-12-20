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


type 'a Environ = EnvDict -> 'a

/// Applicative functor that handles value lookup from submitted form
module Environ = 
    let puree v : 'a Environ = fun (env: EnvDict) -> v
    let ap (f: ('a -> 'b) Environ) (a: 'a Environ) : 'b Environ = 
        fun (env: EnvDict) -> f env (a env)
    let (<*>) f x = ap f x
    let lift f x : 'b Environ = puree f <*> x
    let lift2 f x y : 'c Environ = puree f <*> x <*> y

    let optionalLookup (n: string) : InputValue option Environ = 
        let rec finder env =
            match env with
            | [] -> None
            | x::xs -> 
                let key,value = x
                if key = n
                    then Some value
                    else finder xs
        finder

    let lookup (n: string) : InputValue Environ =
        fun (env: EnvDict) ->
            match optionalLookup n env with
            | None -> failwithf "Key not found in environment: %s" n
            | Some v -> v

    let optionalLookupNonFile (n: string) : string option Environ =
        fun (env: EnvDict) ->
            match optionalLookup n env with
            | Some (Value x) -> Some x
            | Some (File _) -> failwithf "File not expected for key %s" n
            | None -> None

    let lookupNonFile (n: string) : string Environ =
        fun (env: EnvDict) ->
            match lookup n env with
            | Value x -> x
            | File _ -> failwithf "File not expected for key %s" n
