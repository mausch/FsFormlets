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
    let inline mapOrId f =
        function
        | None -> id
        | Some v -> f v

    let inline mapBoolOrId f =
        function
        | Some true -> f
        | _ -> id

    let inline fromBool b = if b then Some() else None

    let inline ap (f: ('a -> 'b) option) (a: 'a option) : 'b option = 
        let (>>=) a f = Option.bind f a
        f >>= fun f' -> a >>= fun a' -> Some (f' a')

    type Builder() =
        member x.Bind a f = Option.bind f a
        member x.Return v = Some v
        member x.Zero() = None

    let builder = Builder()

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

    type Int32 with
        static member tryParse s =
            match Int32.TryParse s with
            | false, _ -> None
            | _, v -> Some v
        static member tryParseHex s = 
            match Int32.TryParse(s, Globalization.NumberStyles.AllowHexSpecifier, Globalization.CultureInfo.InvariantCulture) with
            | false, _ -> None
            | _, v -> Some v

    let (|Length|_|) (l: int) (s: string) =
        if s.Length = l
            then Some()
            else None

    type 'a ISerializer =
        abstract member Serialize: 'a -> string
        abstract member Deserialize: string -> 'a
        abstract member TryDeserialize: string -> (bool * 'a)
    
    open System.IO

    type Stream with
        member x.Rewind() =
            x.Seek(0L, SeekOrigin.Begin) |> ignore

    let losSerializer =
        let f = System.Web.UI.LosFormatter()
        { new ISerializer<obj> with
            member x.Serialize o = 
                use ms = new MemoryStream()
                f.Serialize(ms, o)
                ms.Flush()
                ms.Rewind()
                use r = new StreamReader(ms)
                r.ReadToEnd()
            member x.Deserialize a = 
                if a = null
                    then null
                    else f.Deserialize(a)
            member x.TryDeserialize a = 
                try
                    true, x.Deserialize a
                with _ -> false, null }

    open System.IO.Compression
    open System.Runtime.Serialization
    open System.Xml.Linq

    type XNode with
        static member Parse(x: string) =
            let x = sprintf "<r>%s</r>" x
            let xdoc = XDocument.Parse x
            xdoc.Document.Root.Nodes() |> Seq.toList            

    let binSerializer =
        let ctx = StreamingContext(StreamingContextStates.All)
        let ss = SurrogateSelector()
        let surrogate =
            let k = "v"
            { new ISerializationSurrogate with
                member x.GetObjectData(o, info, ctx) = 
                    info.AddValue(k, o.ToString())
                member x.SetObjectData(o, info, ctx, selector) = 
                    let nodes = info.GetString(k) |> XNode.Parse
                    box nodes.[0] }
        for t in [typeof<XText>; typeof<XElement>; typeof<XNode>] do
            ss.AddSurrogate(t, ctx, surrogate)
        let f = Formatters.Binary.BinaryFormatter(ss, ctx)
        { new ISerializer<obj> with
            member x.Serialize o = 
                using (new MemoryStream()) 
                    (fun ms ->
                        using (new DeflateStream(ms, CompressionMode.Compress, false))
                            (fun cs -> f.Serialize(cs, o))
                        Convert.ToBase64String(ms.ToArray()))
            member x.Deserialize a = 
                if a = null
                    then null
                    else
                        use ms = new MemoryStream(Convert.FromBase64String(a), false)
                        use cs = new DeflateStream(ms, CompressionMode.Decompress, false)
                        f.Deserialize(cs)
            member x.TryDeserialize a = 
                try
                    true, x.Deserialize a
                with _ -> false, null }

    open System.Drawing

    let colorSerializer = 
        { new ISerializer<Color> with
            member x.Serialize c = sprintf "#%02X%02X%02X" c.R c.G c.B
            member x.Deserialize s =
                match x.TryDeserialize s with
                | true, c -> c
                | _ -> failwith "Invalid color"
            member x.TryDeserialize s = 
                let inline (>>=) a f = Option.bind f a
                let checkLength() = 
                    match s with
                    | null -> None
                    | Length 7 -> Some()
                    | _ -> None
                let checkFirst() = if s.[0] = '#' then Some() else None
                let getValue() = s.Substring(1) |> Int32.tryParseHex 
                let color =
                    checkLength() >>= fun() ->
                    checkFirst() >>= fun() ->
                    getValue() >>= fun rgb ->
                    Some (Color.FromArgb rgb)
                match color with
                | Some c -> true, c
                | _ -> false, Color.Black }

    let dateTimeFormats = [|"yyyy-MM-ddTHH:mm:ss.ffZ"; "yyyy-MM-ddTHH:mm:ssZ"; "yyyy-MM-ddTHH:mmZ"|]
    let localDateTimeFormats = [|"yyyy-MM-ddTHH:mm:ss.ff"; "yyyy-MM-ddTHH:mm:ss"; "yyyy-MM-ddTHH:mm"|]
    let dateFormats = [|"yyyy-MM-dd"|]
    let monthFormats = [|"yyyy-MM"|]
    let timeFormats = [|"HH:mm:ss.ff"; "HH:mm:ss"; "HH:mm"|]

    type DateSerialization(formats: string[]) =
        interface DateTime ISerializer with
            member x.Serialize dt = dt.ToString(formats.[0])
            member x.Deserialize dt = 
                DateTime.ParseExact(dt, formats, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AdjustToUniversal)
            member x.TryDeserialize dt = 
                DateTime.TryParseExact(dt, formats, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AdjustToUniversal)

    let localDateTimeSerializer = DateSerialization(localDateTimeFormats) :> ISerializer<_>
    let dateSerializer = DateSerialization(dateFormats) :> ISerializer<_>
    let monthSerializer = DateSerialization(monthFormats) :> ISerializer<_>
    let timeSerializer = DateSerialization(timeFormats) :> ISerializer<_>
    let dateTimeSerializer =
        { new ISerializer<DateTimeOffset> with
            member x.Serialize dt = dt.ToString(dateTimeFormats.[0])
            member x.Deserialize dt = 
                DateTimeOffset.ParseExact(dt, dateTimeFormats, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AssumeUniversal)
            member x.TryDeserialize dt = 
                DateTimeOffset.TryParseExact(dt, dateTimeFormats, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AssumeUniversal) }

    open System.Globalization

    let weekSerializer =
        let culture = CultureInfo.InvariantCulture
        let calendar = culture.Calendar
        let dtFormat = culture.DateTimeFormat
        { new ISerializer<DateTime> with
            member x.Serialize dt =
                // http://www.w3.org/TR/html5/common-microsyntaxes.html#week-number-of-the-last-day
                (* let weeksInYear = 
                    let start = DateTime(dt.Year,1,1)
                    let thursday() = start.DayOfWeek = DayOfWeek.Thursday
                    let wednesdayAnd400() = start.DayOfWeek = DayOfWeek.Wednesday && start.Year % 400 = 0
                    let yearDiv4Not100() = start.Year % 4 = 0 && start.Year % 100 <> 0
                    if thursday() && wednesdayAnd400() && yearDiv4Not100()
                        then 53
                        else 52 *)
                let week = calendar.GetWeekOfYear(dt, CalendarWeekRule.FirstFullWeek, DayOfWeek.Thursday)
                let year =
                    if dt.Month = 1 && week > 4
                        then dt.Year - 1
                        else dt.Year
                sprintf "%d-W%02d" year week
            member x.Deserialize dt = 
                match x.TryDeserialize dt with
                | false, _ -> failwith "Invalid date"
                | _, t -> t
            member x.TryDeserialize dt = 
                let checkLength() = 
                    match dt with
                    | null -> None
                    | Length 8 -> Some()
                    | _ -> None
                let yearParser() = 
                    dt.Substring(0, 4) 
                    |> Int32.tryParse
                    |> Option.bind (fun t -> if t > 0 then Some t else None)
                let sepParser() = 
                    let ok = dt.Substring(4,2) = "-W"
                    Option.fromBool ok
                let weekParser() =
                    dt.Substring(6,2)
                    |> Int32.tryParse
                    |> Option.bind (fun v -> if v >= 1 && v <= 53 then Some v else None)
                let inline (>>=) a f = Option.bind f a
                let v = 
                    checkLength() >>= fun() ->
                    yearParser() >>= fun year ->
                    sepParser() >>= fun() ->
                    weekParser() >>= fun week ->
                    Some (year,week)
                match v with
                | None -> false, DateTime.MinValue
                | Some (year,week) -> true, calendar.AddWeeks(DateTime(year,1,1), week) }

