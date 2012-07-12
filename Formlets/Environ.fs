namespace Formlets

open System
open System.Collections.Generic
open System.Collections.Specialized
open System.Web
open FSharpx

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
    let fromStrings l = 
        let fields = seq { 0..Int32.MaxValue } |> Seq.map (sprintf "%s%d" NameGen.prefix)
        Seq.zip fields l |> fromValueSeq

type 'a Environ = Reader.Reader<EnvDict, 'a>

/// Applicative functor that handles value lookup from submitted form
module Environ = 
    open FSharpx

    let inline puree v : 'a Environ = Reader.returnM v
    let inline ap (a: _ Environ) (f: _ Environ) : _ Environ = Reader.ap a f
    let inline map f x = puree f |> ap x
    let inline lift2 f x y = puree f |> ap x |> ap y

    let lookup (n: string) : InputValue list Environ = 
        fun env ->
            let folder acc elem = 
                let key,value = elem
                if key = n
                    then value::acc
                    else acc
            List.fold folder [] env
