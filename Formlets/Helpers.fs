namespace Formlets

open System

module List =
    open FSharpx

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
        | x -> (List.take x l) @ (item::(List.skip (x+1) l))

module Option =
    let inline mapOrId f =
        function
        | None -> id
        | Some v -> f v

    let inline mapBoolOrId f =
        function
        | Some true -> f
        | _ -> id

[<AutoOpen>]
module Helpers = 
    open FSharpx

    let inline hashs a = (hash a).ToString()

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
        let folder r =
            function
            | "class",v -> addClass v r
            | "style",v -> addStyle v r
            | a -> addOrOverwrite a r
        Seq.fold folder a2 a1

    let isNullOrWhiteSpace (s: string) =
        String.IsNullOrEmpty(s) || Seq.exists Char.IsWhiteSpace s

    type Int32 with
        static member parseHex s = Int32.parseWithOptions Globalization.NumberStyles.AllowHexSpecifier Globalization.CultureInfo.InvariantCulture s

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
                x.TryDeserialize s
                |> Option.fromBoolAndValue 
                |> Option.getOrElseF (fun () -> failwith "Invalid color")

            member x.TryDeserialize s = 
                let checkLength() = 
                    match s with
                    | null -> None
                    | Length 7 -> Some()
                    | _ -> None
                let checkFirst() = if s.[0] = '#' then Some() else None
                let getValue() = s.Substring(1) |> Int32.parseHex 
                let color =
                    Option.maybe {
                        do! checkLength()
                        do! checkFirst()
                        let! rgb = getValue()
                        return Color.FromArgb rgb
                    }
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
                x.TryDeserialize dt 
                |> Option.fromBoolAndValue 
                |> Option.getOrElseF (fun () -> failwith "Invalid date")
            member x.TryDeserialize dt = 
                let checkLength() = 
                    match dt with
                    | null -> None
                    | Length 8 -> Some()
                    | _ -> None
                let yearParser() = 
                    dt.Substring(0, 4) 
                    |> Int32.parse
                    |> Option.bind (fun t -> if t > 0 then Some t else None)
                let sepParser() = 
                    let ok = dt.Substring(4,2) = "-W"
                    Option.fromBool ok
                let weekParser() =
                    dt.Substring(6,2)
                    |> Int32.parse
                    |> Option.bind (fun v -> if v >= 1 && v <= 53 then Some v else None)
                let v = 
                    Option.maybe {
                        do! checkLength()
                        let! year = yearParser()
                        do! sepParser()
                        let! week = weekParser()
                        return year,week
                    }
                match v with
                | None -> false, DateTime.MinValue
                | Some (year,week) -> true, calendar.AddWeeks(DateTime(year,1,1), week) }

