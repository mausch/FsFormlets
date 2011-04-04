module FormletsTests

open TestHelpers
open Xunit
open System
open System.Collections.Specialized
open System.Globalization
open System.Web
open System.Xml.Linq
open Formlets.XmlWriter
open Formlets

let e = FormElements(Validate.Default)

let input = input "" [] // no additional attributes

let inputInt = input |> Validate.Default.Int

let inline fst3 (a,_,_) = a
let inline snd3 (_,b,_) = b
let inline thd3 (_,_,c) = c

let dateFormlet =
    let baseFormlet = 
        div ["style","padding:8px"] (
            span ["style", "border: 2px solid; padding: 4px"] (
                yields t2 <*>
                text "Month: " *> inputInt <*>
                text "Day: " *> inputInt
                <* br <* submit "Send" []
            )
        )
    let isDate (month,day) = 
        DateTime.TryParseExact(sprintf "%d%d%d" 2010 month day, "yyyyMMdd", CultureInfo.InvariantCulture, DateTimeStyles.None) |> fst
    let dateValidator = err isDate (fun (month,day) -> sprintf "%d/%d is not a valid date" month day)
    let validatingFormlet = baseFormlet |> satisfies dateValidator
    map (fun (month,day) -> DateTime(2010, month, day)) validatingFormlet

let fullFormlet =
    span [] (
        yields t8
        <*> dateFormlet
        <*> password
        <*> checkbox false []
        <*> radio "1" ["1","uno"; "2","dos"]
        <*> select "a" ["a","uno"; "b","dos"]
        <*> textarea "" []
        <*> selectMulti ["a";"b"] ["a","uno"; "b","dos"]
        <*> file []
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
    let env = EnvDict.fromValueSeq ["f0", "2"]
    match run radioFormlet env with
    | Success r -> Assert.Equal("2", r)
    | _ -> failwith "shouldn't have failed"

[<Fact>]
let radioRefill() =
    let env = EnvDict.fromValueSeq ["f0", "2"]
    let nth a b = List.nth b a
    let getChildren (n: XNode) =
        match n with
        | Tag e -> e.Nodes() |> Seq.toArray
        | _ -> failwith "Expected tag, got text"
    let r = run radioFormlet env |> fst3 |> nth 0 |> getChildren
    printfn "%A" r
    let input1 = r.[0]
    let input2 = r.[2]
    match input1 with
    | TagA(_,attr,_) -> Assert.False(Seq.exists (fun (k,_) -> k = "checked") attr)
    | _ -> failwith "err"
    match input2 with
    | TagA(_,attr,_) -> Assert.True(Seq.exists (fun (k,_) -> k = "checked") attr)
    | _ -> failwith "err"

[<Fact>]
let checkboxRefill() =
    let formlet = checkbox false []
    let env = EnvDict.fromValueSeq ["f0", "on"]
    let r = run formlet env |> fst3
    printfn "%A" r
    match r.[0] with
    | TagA(_,attr,_) -> Assert.True(Seq.exists (fun (k,_) -> k = "checked") attr)
    | _ -> failwith "err"

[<Fact>]
let inputRefill() =
    let env = EnvDict.fromValueSeq ["f0", "pepe"]
    let r = run input env |> fst3
    printfn "%A" r
    match r.[0] with
    | TagA(_,attr,_) -> Assert.True(Seq.exists (fun (k,v) -> k = "value" && v = "pepe") attr)
    | _ -> failwith "err"

[<Fact>]
let textareaRefill() =
    let env = EnvDict.fromValueSeq ["f0", "pepe"]
    let formlet = textarea "" []
    let r = run formlet env |> fst3
    printfn "%A" r
    match r.[0] with
    | TagA(_,_,content) -> 
        match content with
        | [TextV t] -> Assert.Equal("pepe", t)
        | _ -> failwithf "Unexpected content %A" content
    | _ -> failwith "err"

[<Fact>]
let ``optionalInput refill with value``() =
    let f = Formlet.optionalInput "ovalue" []
    let env = ["f0", "pepe"] |> EnvDict.fromValueSeq
    let r = run f env |> fst3
    let errorForm = XmlWriter.render r
    printfn "%s" errorForm
    Assert.Contains("name=\"f0\" value=\"pepe\"", errorForm)

[<Fact>]
let ``optionalInput refill without value``() =
    let f = Formlet.optionalInput "ovalue" []
    let r,_,v = run f []
    Assert.True v.Value.IsNone
    let errorForm = XmlWriter.render r
    printfn "%s" errorForm
    Assert.Contains("name=\"f0\" value=\"ovalue\"", errorForm)

[<Fact>]
let ``image with values``() =
    let f = Formlet.image "src" "alt" []
    let env = ["f0.x","12"; "f0.y","23"] |> EnvDict.fromValueSeq
    match run f env with
    | Success (Some p) ->
        Assert.Equal(12, p.X)
        Assert.Equal(23, p.Y)
    | _ -> failwith "Should not have failed"

[<Fact>]
let ``image without values``() =
    let f = Formlet.image "src" "alt" []
    let env = [] |> EnvDict.fromValueSeq
    match run f env with
    | Success None -> ()
    | _ -> failwith "Should not have failed"

[<Fact>]
let manualFormletRenderTest() =
    let html = render manualNameFormlet
    printfn "%s" html
    Assert.Equal("<input name=\"somename\" value=\"\" />", html)

[<Fact>]
let manualFormletProcessTest() =
    let env = ["somename", "somevalue"]
    let env = EnvDict.fromValueSeq env
    match run manualNameFormlet env with
    | err,_,Some r ->
        Assert.Equal("somevalue", r)
        match err with
        | [TagA(_,attr,_)] -> Assert.Equal(["name","somename"; "value","somevalue"], attr)
        | _ -> failwithf "Unexpected content %A" err
    | _ -> failwith "Unexpected result"

[<Fact>]
let renderTest() =
    printfn "%s" (render fullFormlet)

[<Fact>]
let processTest() =
    let env = EnvDict.fromValueSeq [
                "f0", "12"
                "f1", "22"
                "f2", ""
                "f4", "1"
                "f5", "b"
                "f6", "blah blah"
                "f7", "a"
                "f7", "b"
              ]
    let filemock = { new HttpPostedFileBase() with
                        member x.ContentLength = 2
                        member x.ContentType = "" }
    let env = env |> EnvDict.addFromFileSeq ["f8", filemock]
    match run fullFormlet env with
    | Success(dt,pass,chk,n,opt,t,many,f) ->
        Assert.Equal(DateTime(2010, 12, 22), dt)
        Assert.Equal("", pass)
        Assert.False chk
        Assert.Equal("1", n)
        Assert.Equal("b", opt)
        Assert.Equal("blah blah", t)
        Assert.Equal(2, many.Length)
        Assert.True(f.IsSome)
    | _ -> failwith "Shouldn't have failed"

[<Fact>]
let processWithInvalidInt() =
    let env = [
                "f0", "aa"
                "f1", "22"
              ]
    let env = EnvDict.fromValueSeq env
    let err,_,value = run dateFormlet env
    printfn "Error form:\n%s" (XmlWriter.render err)
    Assert.True(value.IsNone)

[<Fact>]
let processWithInvalidInts() =
    let env = [
                "f0", "aa"
                "f1", "bb"
              ]
    let env = EnvDict.fromValueSeq env
    let err,_,value = run dateFormlet env
    printfn "Error form:\n%s" (XmlWriter.render err)
    Assert.True(value.IsNone)

[<Fact>]
let processWithInvalidDate() =
    let env = [
                "f0", "22"
                "f1", "22"
              ]
    let env = EnvDict.fromValueSeq env
    let err,_,value = run dateFormlet env
    printfn "Error form:\n%s" (XmlWriter.render err)
    Assert.True(value.IsNone)
    
[<Fact>]
let processWithMissingField() =
    let env = ["f0", "22"] |> EnvDict.fromValueSeq
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

open System.Xml
open System.Xml.Linq

// DSL for XML literals, from http://fssnip.net/U

let inline (!) s = XName.Get(s)
let inline (@=) xn value = XAttribute(xn, value)
let (@?=) xn value = match value with Some s -> XAttribute(xn, s) | None -> null
type XName with 
    member xn.Item 
        with get([<ParamArray>] objs: obj[]) = 
            if objs = null then null else XElement(xn, objs)
     
[<Fact>]
let ``from XElement``() =
    let div = !"div"
    let x = div.[div.["hello", div.[null]], div.["world"]]
    let formlet = xnode x
    let html = render formlet
    Assert.XmlEqual(x, renderToXml formlet)

[<Fact>]
let ``radio with int values``() = 
    let formlet = radioA 5 [2,"dos"; 5,"cinco"]
    let html = render formlet
    printfn "%s" html
    let env = EnvDict.fromValueSeq ["f0","2"]
    match run formlet env with
    | Success v -> Assert.Equal(2,v)
    | _ -> failwith "Shouldn't have failed"

[<Fact>]
let ``radio with record values``() =
    let r1 = { PublicKey = "123123"; PrivateKey = "456456"; MockedResult = None }
    let r2 = { PublicKey = "abc"; PrivateKey = "def"; MockedResult = Some false }
    let formlet = radioA r1 [r1,"dos"; r2,"cinco"]
    let html = render formlet
    printfn "%s" html
    let env = EnvDict.fromValueSeq ["f0",(hash r2).ToString()]
    match run formlet env with
    | Success v -> Assert.Equal(r2,v)
    | _ -> failwith "Shouldn't have failed"

[<Fact>]
let ``validation without xml and with string``() =
    let validator = 
        { IsValid = Int32.TryParse >> fst
          ErrorForm = fun _ b -> b
          ErrorList = fun v -> [sprintf "'%s' is not a valid number" v] }
    let formlet = 
        input 
        |> satisfies validator
        |> map int
    let env = EnvDict.fromValueSeq ["f0","abc"]
    match run formlet env with
    | Success _ -> failwith "Formlet shouldn't have succeeded"
    | Failure(errorForm,errorMsg) -> 
        printfn "Error form: %s" (XmlWriter.render errorForm)
        printfn "%A" errorMsg
        Assert.Equal(1, errorMsg.Length)
        Assert.Equal("'abc' is not a valid number", errorMsg.[0])

[<Fact>]
let ``validation without xml and with string with multiple formlets``() =
    let validator = 
        { IsValid = Int32.TryParse >> fst
          ErrorForm = fun _ b -> b
          ErrorList = fun v -> [sprintf "'%s' is not a valid number" v] }
    let inputInt = 
        input 
        |> satisfies validator
        |> map int
    let formlet = yields t2 <*> inputInt <*> inputInt
    let env = EnvDict.fromValueSeq ["f0","abc"; "f1","def"]
    match run formlet env with
    | Success _ -> failwith "Formlet shouldn't have succeeded"
    | Failure(errorForm,errorMsg) -> 
        printfn "Error form: %s" (XmlWriter.render errorForm)
        printfn "%A" errorMsg
        Assert.Equal(2, errorMsg.Length)
        Assert.Equal("'abc' is not a valid number", errorMsg.[0])
        Assert.Equal("'def' is not a valid number", errorMsg.[1])

[<Fact>]
let ``parse raw xml ``() =
    let formlet = rawXml "something <a href='someurl'>a link</a>"
    let html = render formlet
    printfn "%s" html
    Assert.Equal("something <a href=\"someurl\">a link</a>", html)

[<Fact>]
let ``non-rendering field render``() =
    Assert.Equal("", render field)

[<Fact>]
let ``non-rendering field rendered with another formlet``() =
    let formlet = yields t2 <*> input <*> field
    let html = render formlet
    Assert.Equal("<input name=\"f0\" value=\"\" />", html)

[<Fact>]
let ``non-rendering field run``() =
    let env = EnvDict.fromValueSeq ["f0","def"]
    match run field env with
    | Success v -> Assert.Equal("def", v)
    | _ -> failwith "failed"
    ()

[<Fact>]
let ``validation error in non-rendering field``() =
    let fieldInt = field |> Validate.Default.Int
    let env = EnvDict.fromValueSeq ["f0","def"]
    match run fieldInt env with
    | Success _ -> failwith "Should not have succeeded"
    | Failure(errorForm,errorList) ->
        Assert.Equal(1, errorList.Length)
        Assert.Equal("def is not a valid number", errorList.[0])
        printfn "%s" (XmlWriter.render errorForm)
        //Assert.Equal(0, errorForm.Length)

[<Fact>]
let ``merge attr``() =
    let formlet = input |> mergeAttributes ["id","pepe"]
    let html = render formlet
    Assert.Equal("<input id=\"pepe\" name=\"f0\" value=\"\" />", html)

[<Fact>]
let ``merge attr in error form``() =
    let formlet = input |> mergeAttributes ["id","pepe"] |> Validate.Default.Int
    let env = EnvDict.fromValueSeq ["f0","a"]
    match run formlet env with
    | Failure(errorForm,_) -> 
        let html = XmlWriter.render errorForm
        Assert.Equal("<span class=\"errorinput\"><input id=\"pepe\" name=\"f0\" value=\"a\" /></span><span class=\"error\">a is not a valid number</span>", html)
    | _ -> failwith "Should not have succeeded"

[<Fact>]
let SerializeDateTime() =
    let v = DateTime(2011,1,1, 12,34,56) |> dateTimeSerializer.Serialize
    Assert.Equal("2011-01-01T12:34:56.00Z", v)

[<Fact>]
let ``DateTime ok``() =
    let f = e.DateTime()
    let env = EnvDict.fromValueSeq ["f0","0037-12-13T02:10:33.00Z"]
    match run f env with
    | Success v -> Assert.Equal(DateTime(37,12,13,2,10,33), v)
    | _ -> failwith "should not have failed"

[<Fact>]
let ``DateTime min error``() =
    let f = e.DateTime(min = DateTime(2010,1,1))
    let env = EnvDict.fromValueSeq ["f0","0037-12-13T02:10:33.00Z"]
    match run f env with
    | Success _ -> failwith "should not have succeeded"
    | _ -> ()

[<Fact>]
let ``Date ok``() =
    let f = e.Date()
    let env = EnvDict.fromValueSeq ["f0","0037-12-13"]
    match run f env with
    | Success v -> Assert.Equal(DateTime(37,12,13), v)
    | _ -> failwith "should not have failed"

[<Fact>]
let ``Week serialization``() =
    let w = DateTime(2011,4,3) |> weekSerializer.Serialize
    Assert.Equal("2011-W13", w)

[<Fact>]
let ``Week serialization first day of year``() =
    let w = DateTime(2011,1,1) |> weekSerializer.Serialize
    Assert.Equal("2010-W52", w)

[<Fact>]
let ``Week serialization padded``() =
    let w = DateTime(2011,2,1) |> weekSerializer.Serialize
    Assert.Equal("2011-W04", w)

[<Fact>]
let ``Week deserialization``() =
    let dt = weekSerializer.Deserialize "2011-W13"
    Assert.Equal(DateTime(2011,4,2), dt)

[<Fact>]
let ``Week tryDeserialize fail``() =
    let r = weekSerializer.TryDeserialize "pepe"
    match r with
    | false, _ -> ()
    | _ -> failwith "Should have failed"

[<Fact>]
let ``Time deserialize without second fraction``() =
    let dt = timeSerializer.Deserialize "23:45:56"
    Assert.Equal(23, dt.Hour)
    Assert.Equal(45, dt.Minute)
    Assert.Equal(56, dt.Second)
    Assert.Equal(00, dt.Millisecond)

[<Fact>]
let ``Time deserialize without second``() =
    let dt = timeSerializer.Deserialize "23:45"
    Assert.Equal(23, dt.Hour)
    Assert.Equal(45, dt.Minute)
    Assert.Equal(00, dt.Second)
    Assert.Equal(00, dt.Millisecond)

        