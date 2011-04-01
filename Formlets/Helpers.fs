namespace Formlets

open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameValueCollection =
    open System.Collections.Specialized

    /// <summary>
    /// Returns a new <see cref="NameValueCollection"/> with the concatenation of two <see cref="NameValueCollection"/>s
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let concat a b = 
        let x = NameValueCollection()
        x.Add a
        x.Add b
        x

    /// <summary>
    /// In-place add of a key-value pair to a <see cref="NameValueCollection"/>
    /// </summary>
    /// <param name="x"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let inline add (x: NameValueCollection) (a,b) = x.Add(a,b)

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a sequence of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    let toSeq (a: NameValueCollection) =
        a.AllKeys
        |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a list of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    let inline toList a = toSeq a |> Seq.toList

    /// <summary>
    /// Creates a <see cref="NameValueCollection"/> from a list of key-value pairs
    /// </summary>
    /// <param name="l"></param>
    let fromSeq l =
        let x = NameValueCollection()
        Seq.iter (add x) l
        x

module Seq =
    /// <summary>
    /// Adds an index to a sequence
    /// </summary>
    /// <param name="a"></param>
    let index a = Seq.zip (Seq.initInfinite id) a

    /// <summary>
    /// Returns the first element (with its index) for which the given function returns true.
    /// Return None if no such element exists.
    /// </summary>
    /// <param name="pred">Predicate</param>
    /// <param name="l">Sequence</param>
    let tryFindWithIndex pred l =
        l |> index |> Seq.tryFind (fun (_,v) -> pred v)

    // seq as applicative functor

    let inline puree v = Seq.singleton v
    let inline (<*>) (f: ('a -> 'b) seq) (a: 'a seq): 'b seq =
        let inline bind x = Seq.collect x
        bind (fun f' -> bind (fun a' -> puree (f' a')) a) f
    let inline ap a b = a <*> b

module List =
    let inline singleton a = [a]

    let cons h t = List.Cons(h,t)

    /// <summary>
    /// Returns a list that skips N elements of the underlying list and then yields the remaining elements of the list
    /// </summary>
    /// <param name="i">Elements to skip</param>
    /// <exception cref="ArgumentNullException"/>
    /// <exception cref="InvalidOperationException"/>
    let skip i = Seq.skip i >> Seq.toList

    /// <summary>
    /// Returns the first N elements of the list
    /// </summary>
    /// <param name="i">Elements to take</param>
    /// <exception cref="ArgumentNullException"/>
    /// <exception cref="ArgumentException"/>
    /// <exception cref="InvalidOperationException"/>
    let take i = Seq.take i >> Seq.toList

    /// <summary>
    /// Builds a new list that contains the given subrange specified by starting index and length.
    /// Quite inefficient.
    /// </summary>
    /// <param name="startIndex">The index of the first element of the sublist. </param>
    /// <param name="count">The length of the sublist</param>
    let sub startIndex count = Seq.skip startIndex >> Seq.take count >> Seq.toList
    
    /// <summary>
    /// Replaces an item in a list. Probably horribly inefficient
    /// </summary>
    /// <param name="item">New item</param>
    /// <param name="i">Position in the list to replace</param>
    /// <param name="l">List</param>
    let replaceAt item i l =
        match i with
        | x when x < 0 -> failwith "Out of bounds"
        | x when x >= List.length l -> failwith "Out of bounds"
        | 0 -> item::List.tail l
        | x -> (take x l) @ (item::(skip (x+1) l))

module Option =
    let mapOrId f =
        function
        | None -> id
        | Some v -> f v

    let mapBoolOrId f =
        function
        | Some true -> f
        | _ -> id

[<AutoOpen>]
module Helpers = 
    let inline hashs a = (hash a).ToString()

    /// Builds a pair (2-tuple)
    let inline t2 a b = a,b

    /// Builds a triple (3-tuple)
    let inline t3 a b c = a,b,c

    /// Builds a 4-tuple
    let inline t4 a b c d = a,b,c,d

    /// Builds a 5-tuple
    let inline t5 a b c d e = a,b,c,d,e

    /// Builds a 6-tuple
    let inline t6 a b c d e f = a,b,c,d,e,f

    /// Builds a 7-tuple
    let inline t7 a b c d e f g = a,b,c,d,e,f,g

    /// Builds a 8-tuple
    let inline t8 a b c d e f g h = a,b,c,d,e,f,g,h

    /// <summary>
    /// Appends a value to an existing key in a list of key-value pairs
    /// </summary>
    /// <param name="key">Key to match</param>
    /// <param name="sep">Value separator</param>
    /// <param name="value">Value to append</param>
    /// <param name="attr">List of key-value pairs</param>
    let appendToSameKey key sep value attr =
        match Seq.tryFindWithIndex (fun (k,_) -> k = key) attr with
        | Some (i,(k,v)) -> List.replaceAt (k,v + sep + value) i attr
        | _ -> (key,value)::attr

    /// Appends a class to a list of HTML attributes
    let addClass = appendToSameKey "class" " "

    /// Appends a style to a list of HTML attributes
    let addStyle = appendToSameKey "style" ";"

    /// <summary>
    /// Adds a key-value pair to a list of key-value pairs.
    /// If the key exists in the list, the value is overwritten.
    /// </summary>
    /// <param name="kv">Key-value pair</param>
    /// <param name="attr">list of key-value pairs</param>
    let addOrOverwrite (key,value) attr = 
        match Seq.tryFindWithIndex (fun (k,_) -> k = key) attr with
        | Some (i,(k,v)) -> List.replaceAt (k,value) i attr
        | _ -> (key,value)::attr

    /// <summary>
    /// Merges two lists of HTML attributes.
    /// </summary>
    /// <param name="a1">Original list</param>
    /// <param name="a2">List to merge</param>
    let mergeAttr a1 a2 =
        let folder r a =
            match a with
            | "class",v -> addClass v r
            | "style",v -> addStyle v r
            | _ -> addOrOverwrite a r
        Seq.fold folder a2 a1

    let isNullOrWhiteSpace (s: string) =
        String.IsNullOrEmpty(s) || Seq.exists Char.IsWhiteSpace s

    let dateFormat = "yyyy-MM-ddTHH:mm:ss.ffZ"

    let SerializeDateTime (dt: DateTime) =
        dt.ToString(dateFormat)
        
    let DeserializeDateTime dt =
        DateTime.ParseExact(dt, dateFormat, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AdjustToUniversal)

    let TryDeserializeDateTime dt =
        DateTime.TryParseExact(dt, dateFormat, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AdjustToUniversal)

    let TryDeserializeDateTime' dt =
        let ok,v = TryDeserializeDateTime dt
        match ok,v with
        | false, _ -> None
        | true, v -> Some v
