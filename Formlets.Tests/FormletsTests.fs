module FormletsTests

open Xunit

open System
open System.Collections.Specialized
open System.Globalization
open System.Web
open System.Xml.Linq
open Formlets

let assertThrows<'e when 'e :> exn> f = 
    Assert.Throws<'e>(Assert.ThrowsDelegate(f)) |> ignore

let input = input "" [] // no additional attributes

let inputInt = lift int (input |> Validate.isInt)
//let inputInt = yields (fun i -> int i) <*> (input |> satisfies intValidator) // equivalent to above

let dateFormlet =
    let baseFormlet = 
        div ["style","padding:8px"] (
            span ["style", "border: 2px solid; padding: 4px"] (
(*            puree (fun month day -> DateTime(2010, month, day)) <*>
            text "Month: " *> inputInt <*>
            text "Day: " *> inputInt
*)

            //lift2 (fun month day -> DateTime(2010, month, day)) inputInt inputInt
(*
            lift4 (fun _ month _ day -> DateTime(2010, month, day))
                (text "Month: ")
                inputInt
                (text "Day: ")
                inputInt
*)
(*
                (text "Month: " *> inputInt) ** (text "Day: " *> inputInt)
                |>> fun (month,day) -> DateTime(2010, month, day)
*)
(*
            yields (fun month day -> DateTime(2010, month, day)) <*>
            text "Month: " *> inputInt <*>
            text "Day: " *> inputInt
            <* br <* submit "Send" 
*)

                yields t2 <*>
                text "Month: " *> inputInt <*>
                text "Day: " *> inputInt
                <* br <* submit "Send" 
            )
        )
    let isDate (month,day) = 
        DateTime.TryParseExact(sprintf "%d%d%d" 2010 month day, "yyyyMMdd", CultureInfo.InvariantCulture, DateTimeStyles.None) |> fst
    let dateValidator = err isDate (fun (month,day) -> sprintf "%d/%d is not a valid date" month day)
    let validatingFormlet = baseFormlet |> satisfies dateValidator
    lift (fun (month,day) -> DateTime(2010, month, day)) validatingFormlet

let fullFormlet =
    span [] (
        yields t8
        <*> dateFormlet
        <*> password
        <*> checkbox false
        <*> radio "1" ["1","uno"; "2","dos"]
        <*> select "a" ["a","uno"; "b","dos"]
        <*> textarea "" []
        <*> selectMulti ["a";"b"] ["a","uno"; "b","dos"]
        <*> file
    )

let manualNameFormlet =
    assignedInput "somename" "" []

let radioFormlet = 
    div [] (radio "1" ["1","uno"; "2","dos"])

[<Fact>]
let radioRender() =
    let html = render radioFormlet
    printfn "%s" html

[<Fact>]
let radioRun() =
    let env = EnvDict.fromValueSeq ["input_0", "2"]
    let r = run radioFormlet env |> snd |> Option.get
    Assert.Equal("2", r)

[<Fact>]
let radioRefill() =
    let env = EnvDict.fromValueSeq ["input_0", "2"]
    let nth a b = List.nth b a
    let getChildren =
        function
        | Tag(_,_,c) -> c
        | _ -> failwith "Expected tag, got text"
    let r = run radioFormlet env |> fst |> nth 0 |> getChildren
    printfn "%A" r
    let input1 = r.[0]
    let input2 = r.[2]
    match input1 with
    | Tag(_,attr,_) -> Assert.False(List.exists (fun (k,_) -> k = "checked") attr)
    | _ -> failwith "err"
    match input2 with
    | Tag(_,attr,_) -> Assert.True(List.exists (fun (k,_) -> k = "checked") attr)
    | _ -> failwith "err"

[<Fact>]
let checkboxRefill() =
    let formlet = checkbox false
    let env = EnvDict.fromValueSeq ["input_0", "on"]
    let r = run formlet env |> fst
    printfn "%A" r
    match r.[0] with
    | Tag(_,attr,_) -> Assert.True(List.exists (fun (k,_) -> k = "checked") attr)
    | _ -> failwith "err"

[<Fact>]
let inputRefill() =
    let env = EnvDict.fromValueSeq ["input_0", "pepe"]
    let r = run input env |> fst
    printfn "%A" r
    match r.[0] with
    | Tag(_,attr,_) -> Assert.True(List.exists (fun (k,v) -> k = "value" && v = "pepe") attr)
    | _ -> failwith "err"

[<Fact>]
let textareaRefill() =
    let env = EnvDict.fromValueSeq ["input_0", "pepe"]
    let formlet = textarea "" []
    let r = run formlet env |> fst
    printfn "%A" r
    match r.[0] with
    | Tag(_,_,content) -> 
        match content with
        | [Text s] -> Assert.Equal("pepe", s)
        | x -> failwithf "Unexpected content %A" x
    | _ -> failwith "err"

[<Fact>]
let manualFormletRenderTest() =
    let html = render manualNameFormlet
    printfn "%s" html
    Assert.Equal("<input name=\"somename\" value=\"\" />", html)

[<Fact>]
let manualFormletProcessTest() =
    let env = ["somename", "somevalue"]
    let env = EnvDict.fromValueSeq env
    let r = run manualNameFormlet env
    let err = fst r
    let r = r |> snd |> Option.get
    Assert.Equal("somevalue", r)
    match err with
    | [Tag(_,attr,_)] ->
        Assert.Equal(["name","somename"; "value","somevalue"], attr)
    | x -> failwithf "Unexpected content %A" x

[<Fact>]
let renderTest() =
    printfn "%s" (render fullFormlet)

[<Fact>]
let processTest() =
    let env = EnvDict.fromValueSeq [
                "input_0", "12"
                "input_1", "22"
                "input_2", ""
                "input_4", "1"
                "input_5", "b"
                "input_6", "blah blah"
                "input_7", "a"
                "input_7", "b"
              ]
    let filemock = { new HttpPostedFileBase() with
                        member x.ContentLength = 2
                        member x.ContentType = "" }
    let env = env |> EnvDict.addFromFileSeq ["input_8", filemock]
    let dt,pass,chk,n,opt,t,many,f = run fullFormlet env |> snd |> Option.get
    Assert.Equal(DateTime(2010, 12, 22), dt)
    Assert.Equal("", pass)
    Assert.False chk
    Assert.Equal("1", n)
    Assert.Equal("b", opt)
    Assert.Equal("blah blah", t)
    Assert.Equal(2, many.Length)
    Assert.True(f.IsSome)

[<Fact>]
let processWithInvalidInt() =
    let env = [
                "input_0", "aa"
                "input_1", "22"
              ]
    let env = EnvDict.fromValueSeq env
    let err,value = run dateFormlet env
    let xdoc = XmlWriter.render [Tag("div", [], err)]
    printfn "Error form:\n%s" (xdoc.ToString())
    Assert.True(value.IsNone)

[<Fact>]
let processWithInvalidInts() =
    let env = [
                "input_0", "aa"
                "input_1", "bb"
              ]
    let env = EnvDict.fromValueSeq env
    let err,value = run dateFormlet env
    let xdoc = XmlWriter.render [Tag("div", [], err)]
    printfn "Error form:\n%s" (xdoc.ToString())
    Assert.True(value.IsNone)

[<Fact>]
let processWithInvalidDate() =
    let env = [
                "input_0", "22"
                "input_1", "22"
              ]
    let env = EnvDict.fromValueSeq env
    let err,value = run dateFormlet env
    let xdoc = XmlWriter.render [Tag("div", [], err)]
    printfn "Error form:\n%s" (xdoc.ToString())
    Assert.True(value.IsNone)
    
[<Fact>]
let processWithMissingField() =
    let env = ["input_0", "22"] |> EnvDict.fromValueSeq
    assertThrows<ArgumentException>(fun() -> run dateFormlet env |> ignore)

[<Fact>]
let ``NameValueCollection to seq does not ignore duplicate keys``() =
    let e = NameValueCollection()
    e.Add("1", "one")
    e.Add("1", "uno")
    let values = NameValueCollection.toSeq e
    let values = values |> Seq.filter (fun (k,_) -> k = "1") |> Seq.toList
    Assert.Equal(2, values.Length)

[<Fact>]
let ``input encoded``() =
    let formlet = Formlet.input "<script>" []
    let html = render formlet
    printfn "%s" html
    Assert.Contains("&lt;script&gt;", html)

[<Fact>]
let ``textarea encoded``() =
    let formlet = textarea "<script>" []
    let html = render formlet
    printfn "%s" html
    Assert.Contains("&lt;script&gt;", html)

[<Fact>]
let ``addClass with no previous class``() =
    let before = ["something","value"]
    let after = before |> addClass "aclass"
    Assert.Equal(["class","aclass"; "something","value"], after)

[<Fact>]
let ``addClass with existing class``() =
    let before = ["something","value"; "class","class1"]
    let after = before |> addClass "aclass"
    Assert.Equal(["something","value"; "class","class1 aclass"], after)
    
[<Fact>]
let ``addStyle with no previous style``() =
    let before = ["something","value"]
    let after = before |> addStyle "border: 1px"
    Assert.Equal(["style","border: 1px"; "something","value"], after)
    
[<Fact>]
let ``addStyle with existing style``() =
    let before = ["something","value"; "style","color:red"]
    let after = before |> addStyle "border: 1px"
    Assert.Equal(["something","value"; "style","color:red;border: 1px"], after)
    
[<Fact>]
let ``mergeAttr with no dups``() =
    let a1 = ["something","value"]
    let a2 = ["style","color:red"]
    let r = mergeAttr a1 a2
    printfn "%A" r
    Assert.Equal(2, r.Length)
    Assert.True(r |> List.exists ((=) ("something","value")))
    Assert.True(r |> List.exists ((=) ("style","color:red")))
     
[<Fact>]
let ``mergeAttr with dups``() =
    let a1 = ["something","value"; "else","1"]
    let a2 = ["something","red"]
    let r = a1 |> mergeAttr a2
    printfn "%A" r
    Assert.Equal(2, r.Length)
    Assert.True(r |> List.exists ((=) ("something","red")))
    Assert.True(r |> List.exists ((=) ("else","1")))
     
[<Fact>]
let ``mergeAttr with dup class``() =
    let a1 = ["something","value"; "class","1"]
    let a2 = ["something","red"; "class","bla"]
    let r = a1 |> mergeAttr a2
    printfn "%A" r
    Assert.Equal(2, r.Length)
    Assert.True(r |> List.exists ((=) ("something","red")))
    Assert.True(r |> List.exists ((=) ("class","1 bla")))
     
[<Fact>]
let ``mergeAttr with dup style``() =
    let a1 = ["something","value"; "style","1"]
    let a2 = ["something","red"; "style","bla"]
    let r = a1 |> mergeAttr a2
    printfn "%A" r
    Assert.Equal(2, r.Length)
    Assert.True(r |> List.exists ((=) ("something","red")))
    Assert.True(r |> List.exists ((=) ("style","1;bla")))
     